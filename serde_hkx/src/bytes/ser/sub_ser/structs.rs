use crate::{align, errors::ser::NotFoundPointedPositionSnafu, lib::*, tri};

use super::super::ByteSerializer;
use crate::errors::ser::{Error, Result};
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
    is_root: bool,
}

impl<'a, 'ser> StructSerializer<'a, 'ser> {
    pub const fn new(ser: &'a mut ByteSerializer<'ser>, is_root: bool) -> Self {
        Self { ser, is_root }
    }

    /// processing of array(`hkArray`).
    fn serialize_array_inner<V, T>(
        &mut self,
        array: V,
        size: TypeSize,
        local_src: u32,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        let next_src_pos = self.ser.output.position();

        {
            let pointed_pos = tri!(self.ser.goto_local_dst());
            self.ser.write_local_fixup_pair(local_src, pointed_pos)?;
        }
        let array_base_pos = self.ser.current_last_local_dst;

        // push nest
        let len = array.as_ref().len() as u32;

        match size {
            TypeSize::Struct {
                size_x86,
                size_x86_64,
            } => {
                #[rustfmt::skip]
                let one_size = if self.ser.is_x86 { size_x86 } else { size_x86_64 };
                let nested = self.ser.pointed_pos.len() >= 2; // need align16 or not.

                let write_pointed_pos =
                    calc_array_element_write_pos(array_base_pos, len, one_size, nested, "Struct");
                self.ser.pointed_pos.push(write_pointed_pos);
            }
            TypeSize::String => {
                self.ser.is_in_str_array = true;
                let one_size = if self.ser.is_x86 { 4 } else { 8 };
                let nested = self.ser.pointed_pos.len() >= 2; // need align16 or not.

                let write_pointed_pos =
                    calc_array_element_write_pos(array_base_pos, len, one_size, nested, "String");
                self.ser.pointed_pos.push(write_pointed_pos);
            }
            TypeSize::NonPtr => {}
        }
        #[cfg(feature = "tracing")]
        tracing::trace!("pointed_pos:({:#x?})", self.ser.pointed_pos);

        tri!(array.serialize(&mut *self.ser));
        self.ser.is_in_str_array = false;

        if size == TypeSize::NonPtr {
            let next_pointed_ser_pos = align!(self.ser.output.position(), 16_u64);
            self.ser.current_last_local_dst = next_pointed_ser_pos;
            if let Some(last) = self.ser.pointed_pos.last_mut() {
                *last = next_pointed_ser_pos; // Update to serialize the next pointed data.
            };
        } else {
            // HACK: unused last value to update;
            let pos = tri!(
                self.ser
                    .pointed_pos
                    .pop()
                    .context(NotFoundPointedPositionSnafu)
            );
            let pos = align!(pos, 16_u64);
            if let Some(last) = self.ser.pointed_pos.last_mut() {
                *last = pos;
            };
            self.ser.current_last_local_dst = pos;
        }

        self.ser.output.set_position(next_src_pos); // Go to the next field serialization position.
        Ok(())
    }

    /// Common processing of fixed_array(e.g. `[bool; 3]`)
    fn serialize_array_fixed<V, T>(
        &mut self,
        array: V,
        size: TypeSize,
        local_src: u32,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        // NOTE: struct in fixed array
        // `hkbGeneratorSyncInfo.syncPoints: [hkbGeneratorSyncInfoSyncPoint; 8]` and this is 64 bytes.
        // In other words, embed everything except String types directly into the array.
        // If hkArray is encountered, undefined behavior occurs.
        // However, currently there are no classes in hk2010 that place hkArray into fixed arrays.
        if size == TypeSize::String {
            let pointed_pos = tri!(self.ser.goto_local_dst());
            let array_base_pos = self.ser.current_last_local_dst;
            self.ser.write_local_fixup_pair(local_src, pointed_pos)?;

            self.ser.is_in_str_array = true;
            let one_size = if self.ser.is_x86 { 4 } else { 8 };
            let nested = self.ser.pointed_pos.len() >= 2; // need align16 or not.
            let len = array.as_ref().len() as u32;

            let write_pointed_pos =
                calc_array_element_write_pos(array_base_pos, len, one_size, nested, "String");
            self.ser.pointed_pos.push(write_pointed_pos);
            #[cfg(feature = "tracing")]
            tracing::trace!("pointed_pos:({:#x?})", self.ser.pointed_pos);
        }

        tri!(array.serialize(&mut *self.ser));

        if self.ser.is_in_str_array {
            self.ser.is_in_str_array = false;
            // HACK: unused last value to update;
            let pos = tri!(
                self.ser
                    .pointed_pos
                    .pop()
                    .context(NotFoundPointedPositionSnafu)
            );
            if let Some(last) = self.ser.pointed_pos.last_mut() {
                *last = pos;
            };
        }

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

    #[inline]
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
        let start_relative = tri!(self.ser.relative_position()); // Ptr type need to pointing data position(local.dst).
        self.serialize_array_fixed(value, size, start_relative)
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
        self.serialize_array_inner(value, size, local_src)
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

    #[inline]
    fn end(self) -> Result<()> {
        if self.is_root {
            #[cfg(feature = "tracing")]
            tracing::trace!("pointed_pos:({:#x?})", self.ser.pointed_pos);
            self.ser.pointed_pos.clear();

            #[cfg(feature = "tracing")]
            tracing::trace!("current_last_pos:({:#x?})", self.ser.current_last_local_dst);
            self.ser
                .output
                .set_position(self.ser.current_last_local_dst);
        }
        Ok(())
    }
}

/// Calculate the write position for an array element,
///
/// applying align16 if twice nested.(The reason is unknown. However, it has been determined through hkx binary analysis.)
fn calc_array_element_write_pos(
    array_base_pos: u64,
    len: u32,
    one_size: u64,
    nested: bool,
    _type_kind: &'static str,
) -> u64 {
    let mut write_pointed_pos = array_base_pos + (one_size * len as u64);

    #[cfg(feature = "tracing")]
    tracing::trace!(
        "Calculate hkArray<{_type_kind}> local dst: array_base_pos({array_base_pos:#x}) + one_size({one_size}) * len({len}) = {write_pointed_pos:#x}"
    );

    // NOTE: hkx analysis of `hkArray<hkaAnnotationTrack>` has revealed that align16 is performed when a `hkStringPtr`/`hkCString` is nested twice.
    if nested {
        let new_write_pointed_pos = align!(write_pointed_pos, 16_u64);
        #[cfg(feature = "tracing")]
        tracing::trace!(
            "Apply special align16 because the hkArray<{_type_kind}> is nested twice: {write_pointed_pos:#x} -> {new_write_pointed_pos:#x}"
        );
        write_pointed_pos = new_write_pointed_pos;
    }

    write_pointed_pos
}
