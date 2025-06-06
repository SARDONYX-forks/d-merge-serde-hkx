//! XML Serialization
use crate::{lib::*, tri};

use crate::errors::ser::{Error, Result};
use havok_serde::ser::{
    Serialize, SerializeFlags, SerializeSeq, SerializeStruct, Serializer, TypeSize,
};
use havok_types::variant::Variant;
use havok_types::{
    CString, I8, I16, I32, I64, Matrix3, Matrix4, Pointer, QsTransform, Quaternion, Rotation,
    Signature, StringPtr, Transform, U8, U16, U32, U64, Ulong, Vector4, f16,
};

#[derive(Debug)]
pub struct XmlSerializer {
    /// XML string
    output: String,
    /// Indent type(tab, space)
    indent: &'static str,
    /// Indent time
    depth: usize,
    /// If you want to output XML partially, put [`Option::None`].
    /// # Example XML
    /// ```xml
    /// <?xml version="1.0" encoding="ascii"?>
    /// <hkpackfile classversion="8" contentsversion="hk_2010.2.0-r1" toplevelobject=""
    /// ```
    start_root: Option<&'static str>,
    /// If you want to output XML partially, put [`Option::None`].
    /// # Example XML
    /// ```xml
    /// </hkpackfile>
    /// ```
    end_root: Option<&'static str>,
}

impl Default for XmlSerializer {
    fn default() -> Self {
        Self {
            output: String::new(),
            indent: "\t",
            depth: 0,
            start_root: Some(
                r###"<?xml version="1.0" encoding="ascii"?>
<hkpackfile classversion="8" contentsversion="hk_2010.2.0-r1" toplevelobject=""###,
            ),
            end_root: Some("</hkpackfile>\n"),
        }
    }
}

/// To XML String.
///
/// # Errors
/// serde(fork version) Error defined on crate's trace definition, but will not fail due to mere XML stringing.
#[inline]
pub fn to_string<T>(value: &T, top_ptr: &Pointer<'_>) -> Result<String>
where
    T: Serialize,
{
    to_string_with_opt(value, top_ptr, XmlSerializer::default())
}

/// To xml string with custom `XmlSerializer` settings.
///
/// # Errors
/// serde(fork version) Error defined on crate's trace definition, but will not fail due to mere XML stringing.
///
/// # Info
/// This can be done in partial mode by eliminating the root string.
pub fn to_string_with_opt<T>(value: &T, top_ptr: &Pointer<'_>, ser: XmlSerializer) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = ser;

    if let Some(start_root) = serializer.start_root {
        serializer.output += start_root;
        serializer.output += &top_ptr.to_string();
        serializer.output += "\">\n\n";
        serializer.increment_depth();
        serializer.indent();
        serializer.output += "<hksection name=\"__data__\">\n";
        serializer.increment_depth();
    };

    tri!(value.serialize(&mut serializer));

    if let Some(end_root) = serializer.end_root {
        serializer.decrement_depth();
        serializer.output += "\n";
        serializer.indent();
        serializer.output += "</hksection>";
        serializer.decrement_depth();
        serializer.output += "\n\n";
        serializer.output += end_root;
    };
    Ok(serializer.output)
}

macro_rules! forward_impl_serialize_to_i64 {
    ($($serialize_fn:ident => ($ty:ty, $num:path, $eid:path, $vid:path)),+ $(,)?) => {
        $(
            #[inline]
            fn $serialize_fn(self, v: &$ty) -> Result<Self::Ok> {
                let v = match v {
                    $num(n) => I64::Number(*n as i64),
                    $eid(id) => I64::EventId(id.as_ref().into()),
                    $vid(id) => I64::VariableId(id.as_ref().into()),
                };
                self.serialize_int64(&v)
            }
        )*
    };
}
macro_rules! forward_impl_serialize_to_u64 {
    ($($serialize_fn:ident => ($ty:ty, $num:path, $eid:path, $vid:path)),+ $(,)?) => {
        $(
            #[inline]
            fn $serialize_fn(self, v: &$ty) -> Result<Self::Ok> {
                let v = match v {
                     $num(n) => U64::Number(*n as u64),
                    $eid(id) => U64::EventId(id.as_ref().into()),
                    $vid(id) => U64::VariableId(id.as_ref().into()),
                };
                self.serialize_uint64(&v)
            }
        )*
    };
}

impl Serializer for &mut XmlSerializer {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Self;
    type SerializeStruct = Self;
    type SerializeFlags = Self;

    #[inline]
    fn serialize_void(self, _: ()) -> Result<Self::Ok> {
        Ok(())
    }

    #[inline]
    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.output += if v { "true" } else { "false" };
        Ok(())
    }

    #[inline]
    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.output.push(v);
        Ok(())
    }

    #[inline]
    fn serialize_int8(self, v: &I8) -> Result<Self::Ok> {
        let v = match v {
            I8::Number(n) => I64::Number(*n as i64),
            I8::EventId(id) => I64::EventId(id.as_ref().into()),
            I8::VariableId(id) => I64::VariableId(id.as_ref().into()),
        };
        self.serialize_int64(&v)
    }

    forward_impl_serialize_to_i64! [
    //    serialize_int8 => ( I8,  I8::Number,  I8::EventId,  I8::VariableId),
       serialize_int16 => (I16, I16::Number, I16::EventId, I16::VariableId),
       serialize_int32 => (I32, I32::Number, I32::EventId, I32::VariableId),
    ];
    forward_impl_serialize_to_u64! [
         serialize_uint8 => ( U8,  U8::Number,  U8::EventId,  U8::VariableId),
        serialize_uint16 => (U16, U16::Number, U16::EventId, U16::VariableId),
        serialize_uint32 => (U32, U32::Number, U32::EventId, U32::VariableId),
    ];

    #[inline]
    fn serialize_int64(self, v: &I64) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_uint64(self, v: &U64) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_real(self, v: f32) -> Result<Self::Ok> {
        self.output += &format!("{v:.06}");
        Ok(())
    }

    #[inline]
    fn serialize_vector4(self, v: &Vector4) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_quaternion(self, v: &Quaternion) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_matrix3(self, v: &Matrix3) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_rotation(self, v: &Rotation) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_qstransform(self, v: &QsTransform) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_matrix4(self, v: &Matrix4) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_transform(self, v: &Transform) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_pointer(self, v: &Pointer) -> Result<Self::Ok> {
        if v.is_null() {
            self.output += "null"; // Null pointer
        } else {
            self.output += &v.to_string();
        }
        Ok(())
    }

    #[inline]
    fn serialize_array(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(self)
    }

    /// Create an XML string like the following
    /// ```xml
    /// <hkobject name="#0010" class="hkbProjectData" signature="0x13a39ba7">
    ///   <!-- memSizeAndFlags SERIALIZE_IGNORED -->
    ///   <!-- referenceCount SERIALIZE_IGNORED -->
    ///   <hkparam name="worldUpWS">(0.000000 0.000000 1.000000 0.000000)</hkparam>
    ///   <hkparam name="stringData">#0009</hkparam>
    ///   <hkparam name="defaultEventMode">EVENT_MODE_IGNORE_FROM_GENERATOR</hkparam>
    /// </hkobject>
    /// ```
    fn serialize_struct(
        self,
        name: &'static str,
        class_meta: Option<(&Pointer, Signature)>,
        _sizes: (u64, u64),
    ) -> Result<Self::SerializeStruct> {
        if let Some((ptr_name, sig)) = class_meta {
            self.output += "\n";
            self.indent();
            self.output +=
                &format!("<hkobject name=\"{ptr_name}\" class=\"{name}\" signature=\"{sig}\">\n");
        } else {
            // If there is a line break in the class of a single field, but it is impossible to
            // determine whether it is a class or not within the serialize method of a single
            // field, so if there is no line break here, it is processed as class processing
            // within the field.
            if !self.output.ends_with('\n') {
                self.output += "\n";
                self.increment_depth();
            };
            self.indent();
            self.output += "<hkobject>\n"; // If ptr & signature are not provided, the class is considered to be an in-field class. (e.g. `Array<hkRootContainerNamedVariant>`)
        }

        self.increment_depth(); // entered <hkparam>(each fields process). so we increment indent.
        Ok(self)
    }

    /// FIXME: Unclear XML representation
    #[inline]
    fn serialize_variant(self, v: &Variant) -> Result<Self::Ok> {
        tri!(self.serialize_pointer(&v.object));
        self.serialize_pointer(&v.class)
    }

    #[inline]
    fn serialize_cstring(self, v: &CString) -> Result<Self::Ok> {
        if let Some(s) = v.get_ref() {
            self.output += &html_escape::encode_text(s);
        } else {
            // null is &#9216: https://www.compart.com/en/unicode/U+2400
            self.output += "&#9216;";
        };
        Ok(())
    }

    #[inline]
    fn serialize_ulong(self, v: Ulong) -> Result<Self::Ok> {
        self.output += &v.to_string();
        Ok(())
    }

    #[inline]
    fn serialize_enum_flags(self) -> Result<Self::SerializeFlags> {
        Ok(self)
    }

    #[inline]
    fn serialize_half(self, v: f16) -> Result<Self::Ok> {
        self.output += &format!("{v:.06}");
        Ok(())
    }

    #[inline]
    fn serialize_stringptr(self, v: &StringPtr) -> Result<Self::Ok> {
        if let Some(s) = v.get_ref() {
            self.output += &html_escape::encode_text(s);
        } else {
            // null is &#9216: https://www.compart.com/en/unicode/U+2400
            self.output += "&#9216;";
        };
        Ok(())
    }
}

impl XmlSerializer {
    /// Do indentation by `self.depth`.
    #[inline]
    fn indent(&mut self) {
        match self.depth {
            // Heap alloc optimizations
            0 => (),                                             // 0 alloc
            1 => self.output += self.indent,                     // 1 time alloc
            _ => self.output += &self.indent.repeat(self.depth), // 2 time alloc
        }
    }

    /// Increment `self.depth` for indentation.
    #[inline]
    const fn increment_depth(&mut self) {
        self.depth += 1;
    }

    /// Decrement `self.depth` for indentation.
    #[inline]
    const fn decrement_depth(&mut self) {
        self.depth -= 1;
    }
}

impl SerializeSeq for &mut XmlSerializer {
    type Ok = ();
    type Error = Error;

    /// # Expected XML Examples
    ///
    /// - `hkArray<hkInt8>`(same as ptr, hkReal, etc...)
    /// ```xml
    /// <hkparam name="key" numelements="20">
    ///     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    ///     16 17 18 19 20
    /// </hkparam>
    /// ```
    fn serialize_primitive_element<T>(
        &mut self,
        value: &T,
        index: usize,
        len: usize,
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + havok_serde::ser::Serialize,
    {
        if index == 0 {
            self.indent();
        };
        tri!(value.serialize(&mut **self));

        if index + 1 == len {
            // Align the closing tag of `</hkparam>` by breaking the line at the end of the output,
            // regardless of whether it is every 16 or not.
            self.output.push('\n');
            return Ok(());
        } else if (index + 1) % 16 == 0 {
            self.output.push('\n'); // After 16 outputs, indent and make 16 columns.
            self.indent();
        } else {
            self.output.push(' '); // add space each element.
        }
        Ok(())
    }

    #[inline]
    fn serialize_class_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        tri!(value.serialize(&mut **self));
        self.output += "\n";
        Ok(())
    }

    fn serialize_math_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.indent();
        tri!(value.serialize(&mut **self));
        self.output += "\n";
        Ok(())
    }

    /// # XML Examples
    ///
    /// ```xml
    ///     <hkcstring>CString</hkcstring>
    /// ```
    fn serialize_cstring_element(&mut self, value: &CString) -> Result<()> {
        self.indent();
        self.output += "<hkcstring>";
        tri!(value.serialize(&mut **self));
        self.output += "</hkcstring>\n";
        Ok(())
    }

    /// # XML Examples
    ///
    /// ```xml
    ///     <hkcstring>StringPtr</hkcstring>
    /// ```
    fn serialize_stringptr_element(&mut self, value: &StringPtr) -> Result<()> {
        self.indent();
        self.output += "<hkcstring>";
        tri!(value.serialize(&mut **self));
        self.output += "</hkcstring>\n";
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl SerializeStruct for &mut XmlSerializer {
    type Ok = ();
    type Error = Error;

    /// # XML Examples
    ///
    /// ```xml
    /// <hkparam name="key1">value</hkparam>
    /// ```
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.indent();
        self.output += "<hkparam name=\"";
        self.output += key;
        self.output += "\">";

        tri!(value.serialize(&mut **self));

        // The class in the field is currently having line break processing problems,
        // so it is processed to format it well. from `serialize_struct`
        if self.output.ends_with("</hkobject>") {
            self.output += "\n";
            self.decrement_depth();
            self.indent();
        };
        self.output += "</hkparam>\n";
        Ok(())
    }

    #[inline]
    fn serialize_fixed_array_field<V, T>(
        &mut self,
        key: &'static str,
        value: V,
        size: TypeSize,
    ) -> std::result::Result<(), Self::Error>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        SerializeStruct::serialize_array_field(self, key, value, size)
    }

    /// # XML Examples
    ///
    /// - `hkArray<hkInt8>`(same as ptr, hkReal, etc...)
    /// ```xml
    /// <hkparam name="key" numelements="20">
    ///     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    ///     16 17 18 19 20
    /// </hkparam>
    /// ```
    ///
    /// - `hkArray<hkStringPtr>`
    /// ```xml
    /// <hkparam name="key" numelements="3">
    ///     <hkcstring>StringPtr1</hkcstring>
    ///     <hkcstring>StringPtr2</hkcstring>
    ///     <hkcstring>StringPtr3</hkcstring>
    /// </hkparam>
    /// ```
    ///
    /// - `hkArray<Vector4>`
    /// ```xml
    /// <hkparam name="key" numelements="2">
    ///     (0.000000 0.000000 0.000000 0.000000)
    ///     (0.000000 0.000000 0.000000 0.000000)
    /// </hkparam>
    /// ```
    fn serialize_array_field<V, T>(
        &mut self,
        key: &'static str,
        value: V,
        _size: TypeSize,
    ) -> Result<()>
    where
        V: AsRef<[T]> + Serialize,
        T: Serialize,
    {
        self.indent();
        self.output += "<hkparam name=\"";
        self.output += key;

        let array = value.as_ref();
        if array.is_empty() {
            self.output += "\" numelements=\"0\"></hkparam>\n";
            return Ok(());
        };

        let len = array.len();
        self.output += &format!("\" numelements=\"{len}\">\n");
        self.increment_depth();
        tri!(value.serialize(&mut **self));
        self.decrement_depth();
        self.indent();
        self.output += "</hkparam>\n";
        Ok(())
    }

    /// # XML Examples
    ///
    /// ```xml
    /// <!-- key SERIALIZE_IGNORED --><!-- This is skip_field -->
    /// <hkparam name="otherKey"></hkparam>
    /// ```
    #[inline]
    fn skip_field<T>(&mut self, key: &'static str, _: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.indent();
        self.output += &format!("<!-- {key} SERIALIZE_IGNORED -->\n");
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.decrement_depth();
        self.indent();
        self.output += "</hkobject>";
        Ok(())
    }
}

impl SerializeFlags for &mut XmlSerializer {
    type Ok = ();
    type Error = Error;

    /// e.g. <hkparam>0</hkparam>
    #[inline]
    fn serialize_empty_bit(&mut self) -> Result<(), Self::Error> {
        self.output += "0";
        Ok(())
    }

    #[inline]
    fn serialize_field<T>(&mut self, key: &str, _value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // Always prefix all flags except the first with `|` to indicate an OR operation.
        // e.g. <hkparam>EXAMPLE|EXAMPLE</hkparam>
        if !self.output.ends_with('>') {
            self.output += "|";
        }
        self.output += key;

        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{HavokSort as _, tests::mocks::new_defaultmale};
    use pretty_assertions::assert_eq;

    #[ignore = "No error on local PC Windows, but for some reason error occurs on GitHub Actions Windows"]
    #[test]
    fn test_serialize_defaultmale() -> Result<()> {
        let mut classes = new_defaultmale();
        let top_ptr = classes.sort_for_xml()?; // hkRootContainer" is processed last.
        let actual = tri!(to_string(&classes, &top_ptr));
        let expected =
            include_str!("../../../../docs/handson_hex_dump/defaultmale/defaultmale_x86.xml");

        assert_eq!(actual, expected);
        Ok(())
    }
}
