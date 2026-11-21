use crate::{
    align,
    errors::ser::{NotFoundPointedPositionSnafu, RelArrayOverflowSnafu},
    lib::*,
    tri,
};

use super::super::ByteSerializer;
use crate::errors::ser::{Error, Result};
use byteorder::{BigEndian, LittleEndian, WriteBytesExt as _};
use havok_serde::ser::{Serialize, SerializeStruct, Serializer, TypeSize};
use havok_types::{U32, Ulong};
use snafu::OptionExt as _;
use std::io::Write as _;

/// For bytes struct serializer.
///
/// # Why separate `ByteSerializer`?
/// Avoid mixing `local_fixups` for each field by creating local variables with separate Serializer.
pub struct StructSerializer<'a, 'ser: 'a> {
    ser: &'a mut ByteSerializer<'ser>,
    /// Is it a virtual fixups class? (This indicates it is not a class within a field)
    is_root: bool,
}

impl<'a, 'ser> StructSerializer<'a, 'ser> {
    pub const fn new(ser: &'a mut ByteSerializer<'ser>, is_root: bool) -> Self {
        Self { ser, is_root }
    }

    /// Serialize array elements to the latest pointed data position(after the object), then go back.
    ///
    /// This is the common part of `hkArray<T>`, `[hkStringPtr; N]` and `hkRelArray<T>`.
    ///
    /// 1. Move to the latest pointed data position(align 16).
    /// 2. Call `on_dst(serializer, elements_abs, local_dst)`.(e.g. write a local fixup pair or a relative offset)
    /// 3. Write elements.
    ///    - Struct/String: the data pointed by them is written after all elements.
    /// 4. Update the latest pointed data position & go back to the next field position.
    fn serialize_pointed_elements<V, T, F>(
        &mut self,
        array: V,
        size: TypeSize,
        on_dst: F,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
        F: FnOnce(&mut ByteSerializer<'ser>, u64, u32) -> Result<()>,
    {
        let next_src_pos = self.ser.output.position();

        let (elements_abs, local_dst) = tri!(self.ser.goto_latest_local_dst(true));
        tri!(on_dst(self.ser, elements_abs, local_dst));

        // Struct/String elements may have pointed data.
        let one_size = match size {
            TypeSize::Struct {
                size_x86,
                size_x86_64,
            } => Some((
                if self.ser.is_x86 {
                    size_x86
                } else {
                    size_x86_64
                },
                false,
            )),
            TypeSize::String => Some((if self.ser.is_x86 { 4 } else { 8 }, true)),
            TypeSize::NonPtr => None,
        };
        if let Some((one_size, is_str)) = one_size {
            self.ser.is_in_str_array = is_str;
            // The data pointed by the elements is written after all elements.
            let len = array.as_ref().len() as u64;
            let elements_end = elements_abs + one_size * len;
            // NOTE: The alignment of strings in deep structs is done by `ByteSerializer::serialize_cow`.
            self.ser.pointed_pos.push(elements_end);
        }
        #[cfg(feature = "tracing")]
        tracing::trace!("pointed_pos:({:#x?})", self.ser.pointed_pos);

        tri!(array.serialize(&mut *self.ser)); // serialize elements all
        self.ser.is_in_str_array = false;

        let updated_last_local_dst = if one_size.is_some() {
            // Deletes the ptr write destination for the String/Struct pushed by this function.
            tri!(
                self.ser
                    .pointed_pos
                    .pop()
                    .context(NotFoundPointedPositionSnafu)
            )
        } else {
            self.ser.output.position()
        };
        tri!(
            self.ser
                .update_last_local_dst(align!(updated_last_local_dst, 16_u64))
        );

        self.ser.output.set_position(next_src_pos); // Go back to the next field serialization position.
        Ok(())
    }
}

impl<'a, 'ser> SerializeStruct for StructSerializer<'a, 'ser> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        #[cfg(feature = "tracing")]
        tracing::trace!("serialize field({:#x}): {_key}", self.ser.output.position());
        value.serialize(&mut *self.ser)
    }

    /// Even if it is skipped on XML, it is not skipped because it exists in binary data.
    #[inline]
    fn skip_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_field(key, value)
    }

    fn pad_field<T>(&mut self, x86_pads: &T, x64_pads: &T) -> Result<()>
    where
        T: ?Sized + AsRef<[u8]>,
    {
        let pads = match self.ser.is_x86 {
            true => x86_pads.as_ref(),
            false => x64_pads.as_ref(),
        };

        if pads.is_empty() {
            return Ok(());
        };
        self.ser.output.write_all(pads)?;
        #[cfg(feature = "tracing")]
        {
            let pads_len = pads.len();
            let current_position = self.ser.output.position();
            tracing::trace!("padding: {pads_len} -> current position: {current_position:#x}");
        }
        Ok(())
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Fixed Array

    /// Serialize fixed_array(e.g. `[bool; 3]`)
    ///
    /// # NOTE
    /// `hkbGeneratorSyncInfo.syncPoints: [hkbGeneratorSyncInfoSyncPoint; 8]` and this is 64 bytes.
    /// In other words, embed everything except String types directly into the array.
    ///
    /// # Undefined behavior
    /// If `hkArray` is encountered, undefined behavior occurs.
    /// However, currently there are no classes in hk2010 that place `[hkArray<T>; N]`.
    fn serialize_fixed_array_field<V, T>(
        &mut self,
        _key: &'static str,
        value: V,
        size: TypeSize,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        #[cfg(feature = "tracing")]
        tracing::trace!(
            "serialize FixedArray field({:#x}): {_key}",
            self.ser.output.position()
        );

        if value.as_ref().is_empty() {
            return Ok(());
        }

        // NOTE: In C++, structs within arrays reside on the stack, not the heap. Unlike hkArray,
        //       they are allocated in place because their size affects the allocation.
        if matches!(size, TypeSize::NonPtr | TypeSize::Struct { .. }) {
            return value.serialize(&mut *self.ser); // serialize [T; N] all.
        }

        // TODO: `[hkStringPtr; N]` does not currently exist, so We're not sure if this is correct.
        let local_src = tri!(self.ser.relative_position()); // Ptr type need to pointing data position(local.dst).
        self.serialize_pointed_elements(value, TypeSize::String, |ser, _, local_dst| {
            ser.write_local_fixup_pair(local_src, local_dst)
        })
    }

    #[inline]
    fn skip_fixed_array_field<V, T>(
        &mut self,
        key: &'static str,
        value: V,
        size: TypeSize,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        self.serialize_fixed_array_field(key, value, size)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Array

    /// In the binary serialization of hkx, we are at this stage writing each field of the structure.
    /// ptr type writes only the size of C++ `Array` here, since the data pointed to by the pointer
    /// will be written later.
    ///
    /// That is, ptr(x86: 12bytes, x64: 16bytes).
    fn serialize_array_field<V, T>(
        &mut self,
        _key: &'static str,
        value: V,
        size: TypeSize,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        #[cfg(feature = "tracing")]
        tracing::trace!(
            "serialize Array field({:#x}): {_key}",
            self.ser.output.position()
        );

        let local_src = tri!(self.ser.relative_position()); // Ptr type need to pointing data position(local.dst).
        tri!(self.ser.serialize_ulong(Ulong::new(0))); // ptr size
        let len = value.as_ref().len() as u32;
        tri!(self.ser.serialize_uint32(&U32::Number(len))); // array size
        tri!(self.ser.serialize_uint32(&U32::Number(len | (1 << 31)))); // Capacity(same as size) | Owned flag(32nd bit)

        if len == 0 {
            return Ok(());
        }
        self.serialize_pointed_elements(value, size, |ser, _, local_dst| {
            ser.write_local_fixup_pair(local_src, local_dst)
        })
    }

    #[inline]
    fn skip_array_field<V, T>(&mut self, key: &'static str, value: V, size: TypeSize) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        self.serialize_array_field(key, value, size)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Relative Array(hk2014~)

    /// `hkRelArray<T>` = `{ u16 size; u16 offset; }`(4bytes)
    ///
    /// The elements are placed just after the object like other pointed data, but not pointed by local fixups.
    /// - Each array is aligned 16bytes. (in member order)
    /// - `offset` = elements abs position - this field abs position
    ///
    /// e.g. `hknpConvexPolytopeShape`(size 80) in Fallout4
    /// ```txt
    /// obj+48: vertices { size: 8,  offset: 32  } -> obj+80
    /// obj+64: planes   { size: 8,  offset: 144 } -> obj+208
    /// obj+68: faces    { size: 6,  offset: 268 } -> obj+336
    /// obj+72: indices  { size: 24, offset: 296 } -> obj+368
    /// ```
    fn serialize_rel_array_field<V, T>(
        &mut self,
        key: &'static str,
        value: V,
        size: TypeSize,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        #[cfg(feature = "tracing")]
        tracing::trace!(
            "serialize RelArray field({:#x}): {key}",
            self.ser.output.position()
        );

        let field_abs = self.ser.output.position();
        let len = value.as_ref().len();
        let len = tri!(to_u16(len as u64, key, "size"));
        tri!(write_u16(self.ser, len));
        tri!(write_u16(self.ser, 0)); // offset(placeholder)

        if len == 0 {
            return Ok(());
        }
        self.serialize_pointed_elements(value, size, |ser, elements_abs, _| {
            let offset = tri!(to_u16(elements_abs - field_abs, key, "offset"));
            ser.output.set_position(field_abs + 2);
            tri!(write_u16(ser, offset));
            ser.output.set_position(elements_abs); // back to elements position
            Ok(())
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    #[inline]
    fn end(self) -> Result<()> {
        if !self.is_root {
            self.ser.struct_depth -= 1; // See `ByteSerializer::struct_depth`
        }
        if self.is_root {
            #[cfg(feature = "tracing")]
            tracing::trace!("pointed_pos:({:#x?})", self.ser.pointed_pos);
            // NOTE Write the next virtual fixup after the contents pointed to by the ptr.
            // Otherwise, we'll overwrite it!
            tri!(self.ser.goto_latest_local_dst(true));
            self.ser.pointed_pos.clear();
        }
        Ok(())
    }
}

#[inline]
fn to_u16(value: u64, field: &'static str, what: &'static str) -> Result<u16> {
    u16::try_from(value)
        .ok()
        .context(RelArrayOverflowSnafu { field, what, value })
}

#[inline]
fn write_u16(ser: &mut ByteSerializer<'_>, v: u16) -> Result<()> {
    if ser.is_little_endian {
        ser.output.write_u16::<LittleEndian>(v)?;
    } else {
        ser.output.write_u16::<BigEndian>(v)?;
    }
    Ok(())
}
