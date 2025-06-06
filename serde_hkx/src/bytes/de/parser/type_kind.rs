//! TypeKind bytes parsers
use crate::{lib::*, tri};

use super::BytesStream;
use havok_types::*;
use winnow::binary::{self, Endianness};
use winnow::combinator::{alt, seq, terminated};
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
use winnow::token::take_until;
use winnow::{ModalResult, Parser};

/// Parses [`bool`]. `true` or `false``
/// - The corresponding type kind: `Bool`
///
/// # Errors
/// When parse failed.
#[inline]
pub fn boolean(input: &mut BytesStream<'_>) -> ModalResult<bool> {
    alt((1.value(true), 0.value(false)))
        .context(StrContext::Label("bool(u8)"))
        .context(StrContext::Expected(StrContextValue::Description(
            "`1` or `0`",
        )))
        .parse_next(input)
}

// Unsigned integers -> use `dec_unit`
//   Signed integers -> use `dec_nit`

/// Parses [`f32`](`Real`)
#[inline]
pub fn real<'a>(endian: Endianness) -> impl Parser<BytesStream<'a>, f32, ErrMode<ContextError>> {
    binary::f32(endian)
        .context(StrContext::Label("real(f32)"))
        .context(StrContext::Expected(StrContextValue::Description(
            "Real(e.g. `00 00 80 3f`(0.1))",
        )))
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Math

/// Parse as [`Vector4`]
pub fn vector4<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Vector4, ErrMode<ContextError>> {
    seq!(Vector4 {
        x: real(endian).context(StrContext::Label("x")),
        y: real(endian).context(StrContext::Label("y")),
        z: real(endian).context(StrContext::Label("z")),
        w: real(endian).context(StrContext::Label("w")),
    })
    .context(StrContext::Label("Vector4"))
}

/// Parse as [`Quaternion`]
#[inline]
pub fn quaternion<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Quaternion, ErrMode<ContextError>> {
    move |input: &mut &'a [u8]| {
        let Vector4 { x, y, z, w } = tri!(vector4(endian).parse_next(input));
        Ok(Quaternion { x, y, z, scaler: w })
    }
}

pub fn matrix3<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Matrix3, ErrMode<ContextError>> {
    seq!(Matrix3 {
        x: vector4(endian).context(StrContext::Label("x")),
        y: vector4(endian).context(StrContext::Label("y")),
        z: vector4(endian).context(StrContext::Label("z")),
    })
    .context(StrContext::Label("Matrix3"))
}

pub fn rotation<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Rotation, ErrMode<ContextError>> {
    seq!(Rotation {
        x: vector4(endian).context(StrContext::Label("x")),
        y: vector4(endian).context(StrContext::Label("y")),
        z: vector4(endian).context(StrContext::Label("z")),
    })
    .context(StrContext::Label("Rotation"))
}

pub fn qstransform<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, QsTransform, ErrMode<ContextError>> {
    seq!(QsTransform {
        transition: vector4(endian).context(StrContext::Label("transition")),
        quaternion: quaternion(endian).context(StrContext::Label("quaternion")),
        scale: vector4(endian).context(StrContext::Label("scale")),
    })
    .context(StrContext::Label("QsTransform"))
}

pub fn matrix4<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Matrix4, ErrMode<ContextError>> {
    seq!(Matrix4 {
        x: vector4(endian).context(StrContext::Label("x")),
        y: vector4(endian).context(StrContext::Label("y")),
        z: vector4(endian).context(StrContext::Label("z")),
        w: vector4(endian).context(StrContext::Label("w")),
    })
    .context(StrContext::Label("Matrix4"))
}

pub fn transform<'a>(
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, Transform, ErrMode<ContextError>> {
    seq!(Transform {
        rotation: rotation(endian).context(StrContext::Label("rotation")),
        transition: vector4(endian).context(StrContext::Label("transition")),
    })
    .context(StrContext::Label("Transform"))
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// NOTE: No Pointer parsing exists because it is automatically created as an index.

/// Parses f16
pub fn half<'a>(endian: Endianness) -> impl Parser<BytesStream<'a>, f16, ErrMode<ContextError>> {
    move |bytes: &mut BytesStream<'a>| {
        let (b0, b1) = tri!(
            seq! {
                binary::u8,
                binary::u8,
            }
            .context(StrContext::Label("half(f16)"))
            .context(StrContext::Expected(StrContextValue::Description(
                "half(f16)",
            )))
            .parse_next(bytes)
        );

        Ok(match endian {
            Endianness::Little => f16::from_le_bytes([b0, b1]),
            Endianness::Big => f16::from_be_bytes([b0, b1]),
            Endianness::Native => {
                if cfg!(target_endian = "big") {
                    f16::from_be_bytes([b0, b1])
                } else {
                    #[allow(clippy::tuple_array_conversions)]
                    f16::from_le_bytes([b0, b1]) // if cfg!(target_endian = "little")
                }
            }
        })
    }
}

/// Parses a string literal until `\0`
///
/// # Errors
/// If parse failed.
pub fn string<'a>(input: &mut BytesStream<'a>) -> ModalResult<&'a str> {
    terminated(take_until(0.., b'\0'), b'\0')
        .try_map(|bytes| core::str::from_utf8(bytes))
        .context(StrContext::Label("string"))
        .context(StrContext::Expected(StrContextValue::Description(
            "Valid ASCII string literal",
        )))
        .parse_next(input)
}

/// Parses ptr size(verify 0), size(`int` -> `usize`), and capacity(`int`).
///
/// # Returns
/// (size, capacityAndFlags)
pub fn array_meta<'a>(
    is_x86: bool,
    endian: Endianness,
) -> impl Parser<BytesStream<'a>, (usize, i32), ErrMode<ContextError>> {
    move |bytes: &mut BytesStream<'a>| {
        if is_x86 {
            tri!(
                binary::u32(endian)
                    .verify(|uint| *uint == 0)
                    .map(|uint| uint as u64)
                    .context(StrContext::Expected(StrContextValue::Description(
                        "Skip x86 ptr size(0 fill 4bytes)",
                    )))
                    .parse_next(bytes)
            )
        } else {
            tri!(
                binary::u64(endian)
                    .verify(|ulong| *ulong == 0)
                    .context(StrContext::Expected(StrContextValue::Description(
                        "Skip x64 ptr size(0 fill 8bytes)",
                    )))
                    .parse_next(bytes)
            )
        };

        seq! {
            binary::i32(endian)
            .map(|size| size as usize)
            .context(StrContext::Expected(
                StrContextValue::Description("size(i32)")
            )),
            binary::i32(endian)
            .verify(|cap| (cap & (1 << 31)) != 0) // bit 32th flag is enabled
            .context(
                StrContext::Expected(StrContextValue::Description("capacity&flags(i32: e.g. 00 00 00 80)"))
            )
        }
        .parse_next(bytes)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use zerocopy::IntoBytes as _;

    #[test]
    fn test_boolean() {
        assert_eq!(boolean.parse(&[1]), Ok(true));
        assert_eq!(boolean.parse(&[0]), Ok(false));
        assert!(boolean.parse(b"yes").is_err());
    }

    #[test]
    fn test_vector4() {
        assert_eq!(
            vector4(Endianness::Little).parse([-0.0_f32, 0.0, -0.0, 1.0].as_bytes()),
            Ok(Vector4::new(-0.0, 0.0, -0.0, 1.0))
        );
    }

    #[test]
    fn test_matrix3() {
        assert_eq!(
            matrix3(Endianness::Little).parse(
                [
                    0.0_f32, 0.0, 0.0, 0.0, // 1 vec4
                    -0.0, 0.0, 1.0, 0.0, // 2 vec4
                    1.0, 1.0, 0.0, 0.0, // 3 vec4
                ]
                .as_bytes()
            ),
            Ok(Matrix3 {
                x: Vector4::default(),
                y: Vector4 {
                    x: -0.0,
                    y: 0.0,
                    z: 1.0,
                    w: 0.0
                },
                z: Vector4 {
                    x: 1.0,
                    y: 1.0,
                    z: 0.0,
                    w: 0.0
                }
            })
        );
    }

    #[test]
    fn test_half() {
        assert_eq!(
            half(Endianness::Little).parse(&[0x80, 0x3f]),
            Ok(f16::from_f32(1.0))
        );
        assert_eq!(
            half(Endianness::Big).parse(&[0x3f, 0x80]),
            Ok(f16::from_f32(1.0))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(string.parse(b"example\0"), Ok("example"));
        assert!(string.parse(b"example").is_err());
    }
}
