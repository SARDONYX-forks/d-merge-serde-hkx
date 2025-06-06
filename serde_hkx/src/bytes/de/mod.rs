mod class_index_map;
mod enum_access;
mod map;
pub mod parser;
mod seq;

use crate::{lib::*, tri};

use self::class_index_map::BytesClassIndexMapDeserializer;
use self::enum_access::EnumDeserializer;
use self::map::MapDeserializer;
use self::parser::{
    BytesStream,
    classnames::{ClassNames, classnames_section},
    fixups::Fixups,
    type_kind::{
        array_meta, boolean, matrix3, matrix4, qstransform, quaternion, real, rotation, string,
        transform, vector4,
    },
};
use self::seq::SeqDeserializer;
use super::hexdump::{self, to_hexdump_pos};
use super::serde::{hkx_header::HkxHeader, section_header::SectionHeader};
use crate::errors::{
    de::{Error, Result},
    readable::ReadableError,
};
use havok_serde::de::{self, Deserialize, ReadEnumSize, Visitor};
use havok_types::*;
use winnow::binary::Endianness;
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
use winnow::{Parser, binary};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, educe::Educe)]
#[educe(Default)]
pub struct BytesDeserializer<'de> {
    /// This is readonly bytes data.
    input: &'de [u8],

    /// Binary data position currently being read
    current_position: usize,

    /// Big or Little Endian
    #[educe(Default = Endianness::Little)]
    endian: Endianness,
    /// Is this binary data for 32-bit?
    ///
    /// # Note
    /// This is related to the read size of the pointer type and the skip size of the padding.
    is_x86: bool,

    /// `__classnames__` section contents
    ///
    /// - key: class name start offset
    /// - value: class name
    classnames: ClassNames<'de>,

    /// `__classnames__` header
    classnames_header: SectionHeader,

    /// `__data__` header
    data_header: SectionHeader,
    /// data section fixups
    data_fixups: Fixups,

    /// Unique Class index & XML name attribute(e.g. `#0050`).
    ///
    /// Incremented each time deserialize_struct is called.
    /// # Note
    /// It exists to enable class_index to be retrieved at any time when seq'd as a key in a HashMap, etc.
    class_index: usize,

    /// takeable index for root class (e.g. `#0050`)
    ///
    /// # Intent that field exists.
    /// Provide an [`Option::take`] able index to prevent accidentally giving an index to a class in the structure
    /// or to the parent class of an inheritance source.
    ///
    /// The only place it is incremented is `next_key` in `class_index_map`.
    /// Currently, this means that the index is not incremented except for `HashMap<usize, Classes>`.
    takable_class_index: Option<Pointer<'de>>,
}

impl<'de> BytesDeserializer<'de> {
    /// from xml string
    pub fn from_bytes(input: &'de [u8]) -> Self {
        Self {
            input,
            ..Default::default()
        }
    }
}

/// Parse binary data as the type specified in the partial generics.
///
/// e.g. one class, 3 booleans, `[u32; 10]`,
///
/// # Errors
/// Fail to parse bytes.
///
/// # Note
/// If pointer types are included, it is impossible to deserialize correctly because fixups information is required.
pub fn from_partial_bytes<'a, T>(bytes: &'a [u8]) -> Result<T>
where
    T: Deserialize<'a>,
{
    from_partial_bytes_with_opt(BytesDeserializer::from_bytes(bytes))
}

/// Parse binary data as the type specified in the partial generics with custom `BytesDeserializer` settings.
///
/// e.g. one class, 3 booleans, `[u32; 10]`,
///
/// # Errors
/// Fail to parse bytes.
///
/// # Note
/// If pointer types are included, it is impossible to deserialize correctly because fixups information is required.
pub fn from_partial_bytes_with_opt<'a, T>(de: BytesDeserializer<'a>) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut de = de;
    let t = tri!(T::deserialize(&mut de).map_err(|err| de.to_readable_err(err)));

    if de.input[de.current_position..].is_empty() {
        Ok(t)
    } else {
        Err(de.to_readable_err(Error::TrailingBytes))
    }
}

/// Analyze as binary data of one file in order from hkx header.
///
/// # Errors
/// Fail to parse bytes.
pub fn from_bytes<'a, T>(bytes: &'a [u8]) -> Result<T>
where
    T: Deserialize<'a>,
{
    from_bytes_with_opt(BytesDeserializer::from_bytes(bytes))
}

/// Analyze as binary data of one file in order from hkx header(with custom deserializer settings).
///
/// # Errors
/// Fail to parse bytes.
pub fn from_bytes_with_opt<'a, T>(de: BytesDeserializer<'a>) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut de = de;

    // 1. Deserialize root file header.
    let header = tri!(
        de.parse_peek(HkxHeader::parser())
            .map_err(|err| de.to_readable_err(err))
    );
    de.current_position += 64; // Advance the position by the header size.
    de.is_x86 = header.pointer_size == 4;
    de.endian = header.endian();

    // 2. Deserialize the fixups in the classnames and data sections.
    tri!(
        de.set_section_header_and_fixups(
            header.contents_class_name_section_index,
            header.contents_section_index,
            header.section_count,
        )
        .map_err(|err| de.to_readable_err(err))
    );

    // 3. Parse `__classnames__` section.
    let classnames_abs = de.classnames_header.absolute_data_start as usize;
    let data_abs = de.data_header.absolute_data_start as usize;
    let classnames_section_range = classnames_abs..data_abs; // FIXME: Assumption that `classnames_abs` < `data_abs`
    de.classnames = tri!(
        de.parse_range(classnames_section(de.endian, 0), classnames_section_range)
            .map_err(|err| de.to_readable_err(err))
    );

    // 4. Parse `__data__` section.
    de.current_position = data_abs; // move to data section start
    T::deserialize(&mut de).map_err(|err| de.to_readable_err(err))
}

// SERDE IS NOT A PARSING LIBRARY. This impl block defines a few basic parsing
// functions from scratch. More complicated formats may wish to use a dedicated
// parsing library to help implement their Serde deserializer.
impl<'de> BytesDeserializer<'de> {
    /// Parse by argument parser.
    ///
    /// If an error occurs, it is converted to [`ReadableError`] and returned.
    fn parse_peek<O, P>(&self, mut parser: P) -> Result<O>
    where
        P: Parser<BytesStream<'de>, O, winnow::error::ErrMode<ContextError>>,
    {
        let (_, res) = parser
            .parse_peek(&self.input[self.current_position..])
            .map_err(|err| Error::ContextError { err })?;
        Ok(res)
    }

    /// Parse by argument parser.
    ///
    /// If an error occurs, it is converted to [`Error::ContextError`] and returned.
    fn parse_range<O, P>(&self, mut parser: P, range: Range<usize>) -> Result<O>
    where
        P: Parser<BytesStream<'de>, O, ErrMode<ContextError>>,
    {
        let (_, res) = parser
            .parse_peek(&self.input[range])
            .map_err(|err| Error::ContextError { err })?;
        Ok(res)
    }

    /// Convert Visitor errors to position-assigned errors.
    ///
    /// # Why is this necessary?
    /// Because Visitor errors that occur within each `Deserialize` implementation cannot indicate the error location in bytes.
    #[cold]
    fn to_readable_err(&self, err: Error) -> Error {
        let input = hexdump::to_string(self.input);
        let err_pos = to_hexdump_pos(self.current_position);
        let readable = match err {
            Error::ContextError { err } => ReadableError::from_context(err, input, err_pos),
            Error::ReadableError { source } => source,
            err => ReadableError::from_display(err, input, err_pos),
        };
        Error::ReadableError { source: readable }
    }

    /// Deserialize the fixups in the `classnames` and `data` sections, relying on the information in the root header.
    ///
    /// And, sets fixups to deserializer.
    fn set_section_header_and_fixups(
        &mut self,
        classnames_section_index: i32,
        data_section_index: i32,
        section_len: i32,
    ) -> Result<()> {
        for i in 0..section_len {
            match i {
                i if classnames_section_index == i => {
                    self.classnames_header =
                        tri!(self.parse_peek(SectionHeader::from_bytes(self.endian)));
                    #[cfg(feature = "tracing")]
                    tracing::debug!("classnames_header: {}", self.classnames_header);

                    // NOTE: no fixups
                    // The `classnames` section always has no fixups but its place is filled with abs data.
                }

                i if data_section_index == i => {
                    // 1/4: After parsing the headers, the fixups are parsed, but the position must be returned for the next header read.
                    let backup_pos = self.current_position;

                    // 2/4: read header
                    let header = tri!(self.parse_peek(SectionHeader::from_bytes(self.endian)));

                    // 3/4: read fixups
                    let fixups_start = header.absolute_data_start + header.local_fixups_offset;
                    self.current_position = fixups_start as usize;
                    self.data_fixups =
                        tri!(self.parse_peek(Fixups::from_section_header(&header, self.endian)));

                    #[cfg(feature = "tracing")]
                    tracing::debug!(
                        "data_header: {header},\ndata_fixups: {:#?}",
                        self.data_fixups
                    );
                    self.data_header = header; // Let them be substituted here to avoid ownership issues.

                    // 4/4: back to header position
                    self.current_position = backup_pos;
                }
                _ => {} // Skip unused __types__ section
            };
            self.current_position += 48; // advance section header size(48bytes)
        }
        Ok(())
    }

    /// Get current position(as `global_fixup.src`) -> `global_fixup.dst` -> class index
    fn get_class_index_ptr(&mut self) -> Result<Pointer<'de>> {
        let global_fixup_src = self.relative_position();

        if let Some((_section_index, global_dst)) =
            self.data_fixups.global_fixups.get(&global_fixup_src)
        {
            if let Some(class_index) = self.data_fixups.virtual_fixups.get_index_of(global_dst) {
                #[cfg(feature = "tracing")]
                tracing::debug!(
                    "global_fixup_src: {global_fixup_src}, class_index(from global_dst): {class_index}"
                );

                self.current_position += if self.is_x86 { 4 } else { 8 };
                Ok(Pointer::from_usize(class_index + 1))
            } else {
                #[cfg(feature = "tracing")]
                tracing::debug!(
                    "Missing unique index of class for `global_fixup.dst(virtual_src)`({global_dst}) -> Not found `virtual_fixup.name_offset`. `NullPtr` is entered instead."
                );
                self.current_position += if self.is_x86 { 4 } else { 8 };
                Ok(Pointer::null())
            }
        } else {
            #[cfg(feature = "tracing")]
            tracing::debug!(
                "Not found `global_fixup.src({global_fixup_src})` -> `global_fixup.dst`. `NullPtr` is entered instead."
            );
            self.current_position += if self.is_x86 { 4 } else { 8 };
            Ok(Pointer::null())
        }
    }

    /// Extract the absolute position of the data position pointed to by ptr
    ///
    /// Take the relative position of the `__data__` section at the current position as a key
    /// and extract the corresponding value from the `local_fixups`.
    fn get_local_fixup_dst(&self) -> Result<usize> {
        let local_src = self.relative_position();

        #[cfg(feature = "tracing")]
        {
            let local_src_abs = self.current_position;
            tracing::debug!("local_src: {local_src}/abs({local_src_abs:#x})");
        }

        #[allow(clippy::unnecessary_lazy_evaluations)]
        let local_dst = *tri!({
            self.data_fixups
                .local_fixups
                .get(&local_src)
                .ok_or_else(|| {
                    #[cfg(feature = "tracing")]
                    tracing::debug!("Not found `local_fixup.src({local_src})` -> `local_dst`.");
                    Error::NotFoundDataLocalFixupsValue { key: local_src }
                })
        });

        // Change to abs
        let local_dst_abs = (local_dst + self.data_header.absolute_data_start) as usize;
        #[cfg(feature = "tracing")]
        tracing::debug!("local_dst: {local_dst}/abs({local_dst_abs:#x})");
        Ok(local_dst_abs)
    }

    /// Jump current position(`local_fixup.src`) to dst, then parse, and back to current position.
    fn parse_local_fixup<O, P>(&mut self, parser: P) -> Result<Option<O>>
    where
        P: Parser<BytesStream<'de>, O, winnow::error::ErrMode<ContextError>>,
    {
        let backup_position = self.current_position();
        self.current_position = match self.get_local_fixup_dst().ok() {
            Some(dst) => dst,
            None => return Ok(None),
        };

        let res = tri!(self.parse_peek(parser));

        self.current_position = backup_position as usize;
        Ok(Some(res))
    }

    /// Skip ptr size
    ///
    /// # Errors
    /// Error if the value of ptr to skip is not 0.
    fn skip_ptr_size(&mut self) -> Result<()> {
        if self.is_x86 {
            tri!(
                self.parse_peek(binary::u32(self.endian).verify(|uint| *uint == 0).context(
                    StrContext::Expected(StrContextValue::Description(
                        "Skip x86 ptr size(0 fill 4bytes)"
                    ))
                ))
            );
            self.current_position += 4;
        } else {
            tri!(
                self.parse_peek(
                    binary::u64(self.endian)
                        .verify(|ulong| *ulong == 0)
                        .context(StrContext::Expected(StrContextValue::Description(
                            "Skip x64 ptr size(0 fill 8bytes)"
                        )))
                )
            );
            self.current_position += 8;
        };
        Ok(())
    }

    /// Get current bytes position.
    ///
    /// # Note
    /// This returns [`u32`] to be used as a key to retrieve the data position from the `fixups` that fixes
    /// the data position pointed to by the pointer type.
    #[inline]
    const fn current_position(&self) -> u32 {
        self.current_position as u32
    }

    /// Returns the relative position of the start of data_section as 0.
    ///
    /// # Intent
    /// Use this API when key of fixups requires relative position.
    #[inline]
    const fn relative_position(&self) -> u32 {
        self.current_position() - self.data_header.absolute_data_start
    }
}

// INFO:
// Where did the visit method come from?
// It creates a visit when implementing each Deserialize and reads it. The default is to return an error.
impl<'de> de::Deserializer<'de> for &mut BytesDeserializer<'de> {
    type Error = Error;

    #[inline]
    fn deserialize_identifier<V>(
        self,
        size: ReadEnumSize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_flags(size, visitor)
    }

    // NOTE: This method is never used with bytes because the number of times is controlled by the for loop.
    #[cold]
    fn deserialize_key<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_void(())
    }

    // Deserialize one class.
    fn deserialize_class_index<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_class_index(BytesClassIndexMapDeserializer::new(self))
    }

    #[inline]
    fn deserialize_void<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_void(())
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_bool(tri!(self.parse_peek(boolean)));
        self.current_position += 1;
        res
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_char(tri!(self.parse_peek(
            binary::le_u8.context(StrContext::Expected(StrContextValue::Description("char")))
        )) as char);
        self.current_position += 1;
        res
    }

    fn deserialize_int8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(self.parse_peek(
            binary::le_i8.context(StrContext::Expected(StrContextValue::Description("i8")))
        ));
        let res = visitor.visit_int8(I8::Number(n));
        self.current_position += 1;
        res
    }

    fn deserialize_uint8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(self.parse_peek(
            binary::le_u8.context(StrContext::Expected(StrContextValue::Description("u8")))
        ));
        let res = visitor.visit_uint8(U8::Number(n));
        self.current_position += 1;
        res
    }

    fn deserialize_int16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::i16(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("i16")))
            )
        );
        let res = visitor.visit_int16(I16::Number(n));
        self.current_position += 2;
        res
    }

    fn deserialize_uint16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::u16(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("u16")))
            )
        );
        let res = visitor.visit_uint16(U16::Number(n));
        self.current_position += 2;
        res
    }

    fn deserialize_int32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::i32(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("i32")))
            )
        );
        let res = visitor.visit_int32(I32::Number(n));
        self.current_position += 4;
        res
    }

    fn deserialize_uint32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::u32(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("u32")))
            )
        );
        let res = visitor.visit_uint32(U32::Number(n));
        self.current_position += 4;
        res
    }

    fn deserialize_int64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::i64(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("i64")))
            )
        );
        let res = visitor.visit_int64(I64::Number(n));
        self.current_position += 8;
        res
    }

    fn deserialize_uint64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let n = tri!(
            self.parse_peek(
                binary::u64(self.endian)
                    .context(StrContext::Expected(StrContextValue::Description("u64")))
            )
        );
        let res = visitor.visit_uint64(U64::Number(n));
        self.current_position += 8;
        res
    }

    fn deserialize_real<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_real(tri!(self.parse_peek(
            real(self.endian).context(StrContext::Expected(StrContextValue::Description("f32")))
        )));
        self.current_position += 4;
        res
    }

    fn deserialize_vector4<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_vector4(tri!(self.parse_peek(vector4(self.endian))));
        self.current_position += 16;
        res
    }

    fn deserialize_quaternion<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_quaternion(tri!(self.parse_peek(quaternion(self.endian))));
        self.current_position += 16;
        res
    }

    fn deserialize_matrix3<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_matrix3(tri!(self.parse_peek(matrix3(self.endian))));
        self.current_position += 48;
        res
    }

    fn deserialize_rotation<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_rotation(tri!(self.parse_peek(rotation(self.endian))));
        self.current_position += 48;
        res
    }

    fn deserialize_qstransform<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_qstransform(tri!(self.parse_peek(qstransform(self.endian))));
        self.current_position += 48;
        res
    }

    fn deserialize_matrix4<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_matrix4(tri!(self.parse_peek(matrix4(self.endian))));
        self.current_position += 64;
        res
    }

    fn deserialize_transform<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_transform(tri!(self.parse_peek(transform(self.endian))));
        self.current_position += 64;
        res
    }

    fn deserialize_pointer<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_pointer(tri!(self.get_class_index_ptr()))
    }

    fn deserialize_array<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "tracing")]
        tracing::debug!(
            "current_position: relative({:#x})/abs({:#x})",
            self.relative_position(),
            self.current_position
        );

        // If size is 0, local_fixups does not exist, so check size first.
        // NOTE: This is a look-ahead, assuming the position does not move with this method.
        let (size, _cap_and_flags) = tri!(self.parse_peek(array_meta(self.is_x86, self.endian)));
        #[cfg(feature = "tracing")]
        tracing::debug!("in_struct array_size: {size}");

        if size == 0 {
            self.current_position += if self.is_x86 { 12 } else { 16 };
            visitor.visit_array(SeqDeserializer::new(self, 0))
        } else {
            // The specification requires that the ptr data position be extracted before parsing meta information such as `ptr_size`.
            let pointed_data_position = tri!(self.get_local_fixup_dst());
            self.current_position += if self.is_x86 { 12 } else { 16 }; // NOTE: If we move position before asking for local_fixup, we will not get key correctly.
            let backup_position = self.current_position;

            self.current_position = pointed_data_position;
            let res = visitor.visit_array(SeqDeserializer::new(self, size));
            self.current_position = backup_position;
            res
        }
    }

    // Fixed size array(stack array) is written directly without metadata
    #[inline]
    fn deserialize_fixed_array<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // The fixed size array is controlled by a for loop, so the number of times is not controlled on the deserializer side.
        // Therefore, a dummy is plugged in.
        visitor.visit_array(SeqDeserializer::new(self, usize::MAX))
    }

    fn deserialize_class_index_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let size = self.data_fixups.virtual_fixups.len();

        #[cfg(feature = "tracing")]
        tracing::debug!("class_map_size: {size}");
        visitor.visit_array(SeqDeserializer::new(self, size))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(EnumDeserializer::new(self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_struct_for_bytes(MapDeserializer::new(self, fields))
    }

    /// TODO: binary representation of Variant is unknown.
    fn deserialize_variant<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // hkVariant is never used, and hence always null because the information is unknown.
        let res = visitor.visit_variant(Variant::new(Pointer::null(), Pointer::null()));
        self.current_position += if self.is_x86 { 8 } else { 16 };
        res
    }

    fn deserialize_cstring<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let s = tri!(self.parse_local_fixup(string)).map_or_else(
            || {
                #[cfg(feature = "tracing")]
                tracing::debug!("CString is NullPtr");
                CString::from_option(None)
            },
            CString::from_str,
        );
        tri!(self.skip_ptr_size());
        visitor.visit_cstring(s)
    }

    #[inline]
    fn deserialize_ulong<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.is_x86 {
            self.deserialize_uint32(visitor)
        } else {
            self.deserialize_uint64(visitor)
        }
    }

    fn deserialize_flags<V>(self, size: ReadEnumSize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match size {
            ReadEnumSize::Int8 => self.deserialize_int8(visitor),
            ReadEnumSize::Int16 => self.deserialize_int16(visitor),
            ReadEnumSize::Int32 => self.deserialize_int32(visitor),
            ReadEnumSize::Int64 => self.deserialize_int64(visitor),
            ReadEnumSize::Uint8 => self.deserialize_uint8(visitor),
            ReadEnumSize::Uint16 => self.deserialize_uint16(visitor),
            ReadEnumSize::Uint32 => self.deserialize_uint32(visitor),
            ReadEnumSize::Uint64 => self.deserialize_uint64(visitor),
        }
    }

    fn deserialize_half<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let res = visitor.visit_half(tri!(self.parse_peek(parser::type_kind::half(self.endian))));
        self.current_position += 2;
        res
    }

    fn deserialize_stringptr<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let s = tri!(self.parse_local_fixup(string)).map_or_else(
            || {
                #[cfg(feature = "tracing")]
                tracing::debug!("StringPtr is NullPtr");
                StringPtr::from_option(None)
            },
            StringPtr::from_str,
        );
        tri!(self.skip_ptr_size());
        visitor.visit_stringptr(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::ClassMap;
    use havok_classes::{EventMode, hkBaseObject, hkClassMember_::FlagValues, hkReferencedObject};
    use pretty_assertions::assert_eq;
    use zerocopy::IntoBytes as _;

    fn partial_parse_assert<'a, T>(s: BytesStream<'a>, expected: T)
    where
        T: Deserialize<'a> + PartialEq + fmt::Debug,
    {
        match from_partial_bytes::<T>(s) {
            Ok(res) => assert_eq!(res, expected),
            Err(err) => {
                tracing::error!(?err);
                panic!("{err}")
            }
        }
    }

    #[test]
    fn test_deserialize_primitive() {
        partial_parse_assert(&[128, 0, 0, 0], FlagValues::ALIGN_8);
        partial_parse_assert(&[0], EventMode::EVENT_MODE_DEFAULT);
    }

    #[test]
    fn test_deserialize_primitive_array() {
        partial_parse_assert::<[char; 0]>(b"", []);
        partial_parse_assert(&[1, 0], [true, false]);
        partial_parse_assert(
            [
                0_u32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            ]
            .as_bytes(),
            core::array::from_fn::<U32, 21, _>(|i| U32::Number(i as u32)),
        );
    }

    #[test]
    fn test_deserialize_math_array() {
        #[rustfmt::skip]
        let expected = [
            Vector4 { x: -0.0, y: 0.0, z: -0.0, w: 1.0, },
            Vector4 { x:  0.0, y: 0.0, z: -0.0, w: 1.0, },
            Vector4 { x: -0.0, y: 0.0, z: -0.0, w: 1.0, },
        ];
        partial_parse_assert(
            [
                -0.0_f32, 0.0, -0.0, 1.0, // 1 vec4
                0.0, 0.0, -0.0, 1.0, // 2 vec4
                -0.0, 0.0, -0.0, 1.0, // 3 vec4
            ]
            .as_bytes(),
            expected,
        );
    }

    #[test]
    fn test_deserialize_class() {
        partial_parse_assert(
            &[
                0, 0, 0, 0, 0, 0, 0, 0, // hkBaseObject
                2, 0, // mem_size_and_flags
                0, 0, // reference_count
                0, 0, 0, 0, // 8bytes align for struct
            ],
            hkReferencedObject {
                __ptr: None, // In single class partial mode, ptr is not allocated.
                parent: hkBaseObject { __ptr: None },
                m_memSizeAndFlags: U16::Number(2),
                m_referenceCount: I16::Number(0),
            },
        );
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    fn from_file<'a, T>(bytes: &'a [u8]) -> T
    where
        T: Deserialize<'a>,
    {
        match from_bytes::<T>(bytes) {
            Ok(res) => res,
            Err(err) => {
                tracing::error!("{err}");
                panic!("{err}")
            }
        }
    }

    #[test]
    #[cfg_attr(
        all(feature = "tracing", not(miri)),
        quick_tracing::init(test = "deserialize_hkx_bytes", stdio = false)
    )]
    fn test_deserialize_class_index() {
        let bytes = include_bytes!("../../../../docs/handson_hex_dump/defaultmale/defaultmale.hkx");
        let actual = from_file::<ClassMap>(bytes);
        assert!(actual.len() == 3);
    }
}
