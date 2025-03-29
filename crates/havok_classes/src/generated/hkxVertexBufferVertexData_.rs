use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkxVertexBufferVertexData`
/// - version: `0`
/// - signature: `0xd72b6fd0`
/// - size: ` 84`(x86)/`104`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkxVertexBufferVertexData<'a> {
    /// # Unique index for this class
    /// - Represents a pointer on XML (`<hkobject name="#0001"></hkobject>`)
    /// - [`Option::None`] => This class is `class in field`.(`<hkobject></hkobject>`)
    ///
    /// # Note
    /// Not present in the binary & Not exist actual C++ field.
    #[cfg_attr(
        feature = "serde",
        serde(skip_serializing_if = "Option::is_none", default)
    )]
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub __ptr: Option<Pointer<'a>>,
    /// # C++ Info
    /// - name: `vectorData`(ctype: `hkArray<hkVector4>`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "vectorData"))]
    #[cfg_attr(feature = "serde", serde(rename = "vectorData"))]
    pub m_vectorData: Vec<Vector4>,
    /// # C++ Info
    /// - name: `floatData`(ctype: `hkArray<hkReal>`)
    /// - offset: ` 12`(x86)/` 16`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "floatData"))]
    #[cfg_attr(feature = "serde", serde(rename = "floatData"))]
    pub m_floatData: Vec<f32>,
    /// # C++ Info
    /// - name: `uint32Data`(ctype: `hkArray<hkUint32>`)
    /// - offset: ` 24`(x86)/` 32`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint32Data"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint32Data"))]
    pub m_uint32Data: Vec<U32<'a>>,
    /// # C++ Info
    /// - name: `uint16Data`(ctype: `hkArray<hkUint16>`)
    /// - offset: ` 36`(x86)/` 48`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint16Data"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint16Data"))]
    pub m_uint16Data: Vec<U16<'a>>,
    /// # C++ Info
    /// - name: `uint8Data`(ctype: `hkArray<hkUint8>`)
    /// - offset: ` 48`(x86)/` 64`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint8Data"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint8Data"))]
    pub m_uint8Data: Vec<U8<'a>>,
    /// # C++ Info
    /// - name: `numVerts`(ctype: `hkUint32`)
    /// - offset: ` 60`(x86)/` 80`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "numVerts"))]
    #[cfg_attr(feature = "serde", serde(rename = "numVerts"))]
    pub m_numVerts: U32<'a>,
    /// # C++ Info
    /// - name: `vectorStride`(ctype: `hkUint32`)
    /// - offset: ` 64`(x86)/` 84`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "vectorStride"))]
    #[cfg_attr(feature = "serde", serde(rename = "vectorStride"))]
    pub m_vectorStride: U32<'a>,
    /// # C++ Info
    /// - name: `floatStride`(ctype: `hkUint32`)
    /// - offset: ` 68`(x86)/` 88`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "floatStride"))]
    #[cfg_attr(feature = "serde", serde(rename = "floatStride"))]
    pub m_floatStride: U32<'a>,
    /// # C++ Info
    /// - name: `uint32Stride`(ctype: `hkUint32`)
    /// - offset: ` 72`(x86)/` 92`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint32Stride"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint32Stride"))]
    pub m_uint32Stride: U32<'a>,
    /// # C++ Info
    /// - name: `uint16Stride`(ctype: `hkUint32`)
    /// - offset: ` 76`(x86)/` 96`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint16Stride"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint16Stride"))]
    pub m_uint16Stride: U32<'a>,
    /// # C++ Info
    /// - name: `uint8Stride`(ctype: `hkUint32`)
    /// - offset: ` 80`(x86)/`100`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uint8Stride"))]
    #[cfg_attr(feature = "serde", serde(rename = "uint8Stride"))]
    pub m_uint8Stride: U32<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkxVertexBufferVertexData<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkxVertexBufferVertexData"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xd72b6fd0)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v
        }
    }
    impl<'a> _serde::Serialize for hkxVertexBufferVertexData<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xd72b6fd0)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkxVertexBufferVertexData",
                    class_meta,
                    (84u64, 104u64),
                )?;
            serializer
                .serialize_array_field(
                    "vectorData",
                    &self.m_vectorData,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "floatData",
                    &self.m_floatData,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "uint32Data",
                    &self.m_uint32Data,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "uint16Data",
                    &self.m_uint16Data,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "uint8Data",
                    &self.m_uint8Data,
                    TypeSize::NonPtr,
                )?;
            serializer.serialize_field("numVerts", &self.m_numVerts)?;
            serializer.serialize_field("vectorStride", &self.m_vectorStride)?;
            serializer.serialize_field("floatStride", &self.m_floatStride)?;
            serializer.serialize_field("uint32Stride", &self.m_uint32Stride)?;
            serializer.serialize_field("uint16Stride", &self.m_uint16Stride)?;
            serializer.serialize_field("uint8Stride", &self.m_uint8Stride)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkxVertexBufferVertexData<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_vectorData,
                m_floatData,
                m_uint32Data,
                m_uint16Data,
                m_uint8Data,
                m_numVerts,
                m_vectorStride,
                m_floatStride,
                m_uint32Stride,
                m_uint16Stride,
                m_uint8Stride,
                __ignore,
            }
            struct __FieldVisitor;
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(__formatter, "field identifier")
                }
                /// Intended for use in XML.
                #[allow(clippy::match_single_binding)]
                #[allow(clippy::reversed_empty_ranges)]
                #[allow(clippy::single_match)]
                fn visit_key<__E>(
                    self,
                    __value: &str,
                ) -> core::result::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "vectorData" => Ok(__Field::m_vectorData),
                        "floatData" => Ok(__Field::m_floatData),
                        "uint32Data" => Ok(__Field::m_uint32Data),
                        "uint16Data" => Ok(__Field::m_uint16Data),
                        "uint8Data" => Ok(__Field::m_uint8Data),
                        "numVerts" => Ok(__Field::m_numVerts),
                        "vectorStride" => Ok(__Field::m_vectorStride),
                        "floatStride" => Ok(__Field::m_floatStride),
                        "uint32Stride" => Ok(__Field::m_uint32Stride),
                        "uint16Stride" => Ok(__Field::m_uint16Stride),
                        "uint8Stride" => Ok(__Field::m_uint8Stride),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(
                    __deserializer: __D,
                ) -> core::result::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_key(__deserializer, __FieldVisitor)
                }
            }
            struct __hkxVertexBufferVertexDataVisitor<'de> {
                marker: _serde::__private::PhantomData<hkxVertexBufferVertexData<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkxVertexBufferVertexDataVisitor<'de> {
                type Value = hkxVertexBufferVertexData<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkxVertexBufferVertexData",
                    )
                }
                fn visit_struct_for_bytes<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::__private::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let __ptr = __A::class_ptr(&mut __map);
                    let mut m_vectorData: _serde::__private::Option<Vec<Vector4>> = _serde::__private::None;
                    let mut m_floatData: _serde::__private::Option<Vec<f32>> = _serde::__private::None;
                    let mut m_uint32Data: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_uint16Data: _serde::__private::Option<Vec<U16<'de>>> = _serde::__private::None;
                    let mut m_uint8Data: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_numVerts: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_vectorStride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_floatStride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint32Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint16Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint8Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    for i in 0..11usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_vectorData) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vectorData",
                                        ),
                                    );
                                }
                                m_vectorData = _serde::__private::Some(
                                    match __A::next_value::<Vec<Vector4>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_floatData) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "floatData",
                                        ),
                                    );
                                }
                                m_floatData = _serde::__private::Some(
                                    match __A::next_value::<Vec<f32>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_uint32Data) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint32Data",
                                        ),
                                    );
                                }
                                m_uint32Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_uint16Data) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint16Data",
                                        ),
                                    );
                                }
                                m_uint16Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_uint8Data) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint8Data",
                                        ),
                                    );
                                }
                                m_uint8Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(&m_numVerts) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numVerts",
                                        ),
                                    );
                                }
                                m_numVerts = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(&m_vectorStride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vectorStride",
                                        ),
                                    );
                                }
                                m_vectorStride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(&m_floatStride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "floatStride",
                                        ),
                                    );
                                }
                                m_floatStride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            8usize => {
                                if _serde::__private::Option::is_some(&m_uint32Stride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint32Stride",
                                        ),
                                    );
                                }
                                m_uint32Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            9usize => {
                                if _serde::__private::Option::is_some(&m_uint16Stride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint16Stride",
                                        ),
                                    );
                                }
                                m_uint16Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            10usize => {
                                if _serde::__private::Option::is_some(&m_uint8Stride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint8Stride",
                                        ),
                                    );
                                }
                                m_uint8Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    let m_vectorData = match m_vectorData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vectorData",
                                ),
                            );
                        }
                    };
                    let m_floatData = match m_floatData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "floatData",
                                ),
                            );
                        }
                    };
                    let m_uint32Data = match m_uint32Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint32Data",
                                ),
                            );
                        }
                    };
                    let m_uint16Data = match m_uint16Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint16Data",
                                ),
                            );
                        }
                    };
                    let m_uint8Data = match m_uint8Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint8Data",
                                ),
                            );
                        }
                    };
                    let m_numVerts = match m_numVerts {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("numVerts"),
                            );
                        }
                    };
                    let m_vectorStride = match m_vectorStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vectorStride",
                                ),
                            );
                        }
                    };
                    let m_floatStride = match m_floatStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "floatStride",
                                ),
                            );
                        }
                    };
                    let m_uint32Stride = match m_uint32Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint32Stride",
                                ),
                            );
                        }
                    };
                    let m_uint16Stride = match m_uint16Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint16Stride",
                                ),
                            );
                        }
                    };
                    let m_uint8Stride = match m_uint8Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint8Stride",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkxVertexBufferVertexData {
                        __ptr,
                        m_vectorData,
                        m_floatData,
                        m_uint32Data,
                        m_uint16Data,
                        m_uint8Data,
                        m_numVerts,
                        m_vectorStride,
                        m_floatStride,
                        m_uint32Stride,
                        m_uint16Stride,
                        m_uint8Stride,
                    })
                }
                #[allow(clippy::manual_unwrap_or_default)]
                fn visit_struct<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::__private::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut m_vectorData: _serde::__private::Option<Vec<Vector4>> = _serde::__private::None;
                    let mut m_floatData: _serde::__private::Option<Vec<f32>> = _serde::__private::None;
                    let mut m_uint32Data: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_uint16Data: _serde::__private::Option<Vec<U16<'de>>> = _serde::__private::None;
                    let mut m_uint8Data: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_numVerts: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_vectorStride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_floatStride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint32Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint16Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_uint8Stride: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_vectorData => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_vectorData) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vectorData",
                                        ),
                                    );
                                }
                                m_vectorData = _serde::__private::Some(
                                    match __A::next_value::<Vec<Vector4>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_floatData => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_floatData) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "floatData",
                                        ),
                                    );
                                }
                                m_floatData = _serde::__private::Some(
                                    match __A::next_value::<Vec<f32>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint32Data => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint32Data) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint32Data",
                                        ),
                                    );
                                }
                                m_uint32Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint16Data => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint16Data) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint16Data",
                                        ),
                                    );
                                }
                                m_uint16Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint8Data => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint8Data) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint8Data",
                                        ),
                                    );
                                }
                                m_uint8Data = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_numVerts => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_numVerts) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numVerts",
                                        ),
                                    );
                                }
                                m_numVerts = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_vectorStride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_vectorStride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vectorStride",
                                        ),
                                    );
                                }
                                m_vectorStride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_floatStride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_floatStride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "floatStride",
                                        ),
                                    );
                                }
                                m_floatStride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint32Stride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint32Stride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint32Stride",
                                        ),
                                    );
                                }
                                m_uint32Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint16Stride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint16Stride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint16Stride",
                                        ),
                                    );
                                }
                                m_uint16Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uint8Stride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uint8Stride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "uint8Stride",
                                        ),
                                    );
                                }
                                m_uint8Stride = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            _ => __A::skip_value(&mut __map)?,
                        }
                    }
                    let m_vectorData = match m_vectorData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vectorData",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_floatData = match m_floatData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "floatData",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint32Data = match m_uint32Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint32Data",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint16Data = match m_uint16Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint16Data",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint8Data = match m_uint8Data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint8Data",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_numVerts = match m_numVerts {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("numVerts"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_vectorStride = match m_vectorStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vectorStride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_floatStride = match m_floatStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "floatStride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint32Stride = match m_uint32Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint32Stride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint16Stride = match m_uint16Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint16Stride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uint8Stride = match m_uint8Stride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "uint8Stride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkxVertexBufferVertexData {
                        __ptr: __ptr.clone(),
                        m_vectorData,
                        m_floatData,
                        m_uint32Data,
                        m_uint16Data,
                        m_uint8Data,
                        m_numVerts,
                        m_vectorStride,
                        m_floatStride,
                        m_uint32Stride,
                        m_uint16Stride,
                        m_uint8Stride,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "vectorData",
                "floatData",
                "uint32Data",
                "uint16Data",
                "uint8Data",
                "numVerts",
                "vectorStride",
                "floatStride",
                "uint32Stride",
                "uint16Stride",
                "uint8Stride",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkxVertexBufferVertexData",
                FIELDS,
                __hkxVertexBufferVertexDataVisitor {
                    marker: _serde::__private::PhantomData::<hkxVertexBufferVertexData>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
