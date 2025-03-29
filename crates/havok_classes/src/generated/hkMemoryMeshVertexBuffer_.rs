use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkMemoryMeshVertexBuffer`
/// - version: `1`
/// - signature: `0xa2e50753`
/// - size: `424`(x86)/`440`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkMemoryMeshVertexBuffer<'a> {
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
    /// Alternative to C++ class inheritance.
    #[cfg_attr(feature = "json_schema", schemars(flatten))]
    #[cfg_attr(feature = "serde", serde(flatten))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub parent: hkMeshVertexBuffer<'a>,
    /// # C++ Info
    /// - name: `format`(ctype: `struct hkVertexFormat`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: `260`(x86)/`260`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "format"))]
    #[cfg_attr(feature = "serde", serde(rename = "format"))]
    pub m_format: hkVertexFormat<'a>,
    /// # C++ Info
    /// - name: `elementOffsets`(ctype: `hkInt32[32]`)
    /// - offset: `268`(x86)/`276`(x86_64)
    /// - type_size: `128`(x86)/`128`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "elementOffsets"))]
    #[cfg_attr(feature = "serde", serde(rename = "elementOffsets"))]
    pub m_elementOffsets: [I32<'a>; 32usize],
    /// # C++ Info
    /// - name: `memory`(ctype: `hkArray<hkUint8>`)
    /// - offset: `396`(x86)/`408`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "memory"))]
    #[cfg_attr(feature = "serde", serde(rename = "memory"))]
    pub m_memory: Vec<U8<'a>>,
    /// # C++ Info
    /// - name: `vertexStride`(ctype: `hkInt32`)
    /// - offset: `408`(x86)/`424`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "vertexStride"))]
    #[cfg_attr(feature = "serde", serde(rename = "vertexStride"))]
    pub m_vertexStride: I32<'a>,
    /// # C++ Info
    /// - name: `locked`(ctype: `hkBool`)
    /// - offset: `412`(x86)/`428`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "locked"))]
    #[cfg_attr(feature = "serde", serde(rename = "locked"))]
    pub m_locked: bool,
    /// # C++ Info
    /// - name: `numVertices`(ctype: `hkInt32`)
    /// - offset: `416`(x86)/`432`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "numVertices"))]
    #[cfg_attr(feature = "serde", serde(rename = "numVertices"))]
    pub m_numVertices: I32<'a>,
    /// # C++ Info
    /// - name: `isBigEndian`(ctype: `hkBool`)
    /// - offset: `420`(x86)/`436`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "isBigEndian"))]
    #[cfg_attr(feature = "serde", serde(rename = "isBigEndian"))]
    pub m_isBigEndian: bool,
    /// # C++ Info
    /// - name: `isSharable`(ctype: `hkBool`)
    /// - offset: `421`(x86)/`437`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "isSharable"))]
    #[cfg_attr(feature = "serde", serde(rename = "isSharable"))]
    pub m_isSharable: bool,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkMemoryMeshVertexBuffer<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkMemoryMeshVertexBuffer"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xa2e50753)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_format.deps_indexes());
            v
        }
    }
    impl<'a> _serde::Serialize for hkMemoryMeshVertexBuffer<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xa2e50753)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkMemoryMeshVertexBuffer",
                    class_meta,
                    (424u64, 440u64),
                )?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer
                .skip_field("memSizeAndFlags", &self.parent.parent.m_memSizeAndFlags)?;
            serializer
                .skip_field("referenceCount", &self.parent.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.serialize_field("format", &self.m_format)?;
            serializer
                .serialize_fixed_array_field(
                    "elementOffsets",
                    self.m_elementOffsets.as_slice(),
                    TypeSize::NonPtr,
                )?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer
                .serialize_array_field("memory", &self.m_memory, TypeSize::NonPtr)?;
            serializer.serialize_field("vertexStride", &self.m_vertexStride)?;
            serializer.serialize_field("locked", &self.m_locked)?;
            serializer.pad_field([0u8; 3usize].as_slice(), [0u8; 3usize].as_slice())?;
            serializer.serialize_field("numVertices", &self.m_numVertices)?;
            serializer.serialize_field("isBigEndian", &self.m_isBigEndian)?;
            serializer.serialize_field("isSharable", &self.m_isSharable)?;
            serializer.pad_field([0u8; 2usize].as_slice(), [0u8; 2usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkMemoryMeshVertexBuffer<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_format,
                m_elementOffsets,
                m_memory,
                m_vertexStride,
                m_locked,
                m_numVertices,
                m_isBigEndian,
                m_isSharable,
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
                        "format" => Ok(__Field::m_format),
                        "elementOffsets" => Ok(__Field::m_elementOffsets),
                        "memory" => Ok(__Field::m_memory),
                        "vertexStride" => Ok(__Field::m_vertexStride),
                        "locked" => Ok(__Field::m_locked),
                        "numVertices" => Ok(__Field::m_numVertices),
                        "isBigEndian" => Ok(__Field::m_isBigEndian),
                        "isSharable" => Ok(__Field::m_isSharable),
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
            struct __hkMemoryMeshVertexBufferVisitor<'de> {
                marker: _serde::__private::PhantomData<hkMemoryMeshVertexBuffer<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkMemoryMeshVertexBufferVisitor<'de> {
                type Value = hkMemoryMeshVertexBuffer<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkMemoryMeshVertexBuffer",
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
                    let parent = __A::parent_value(&mut __map)?;
                    let mut m_format: _serde::__private::Option<hkVertexFormat<'de>> = _serde::__private::None;
                    let mut m_elementOffsets: _serde::__private::Option<
                        [I32<'de>; 32usize],
                    > = _serde::__private::None;
                    let mut m_memory: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_vertexStride: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_locked: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_numVertices: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_isBigEndian: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_isSharable: _serde::__private::Option<bool> = _serde::__private::None;
                    for i in 0..8usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_format) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("format"),
                                    );
                                }
                                m_format = _serde::__private::Some(
                                    match __A::next_value::<hkVertexFormat<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_elementOffsets) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elementOffsets",
                                        ),
                                    );
                                }
                                m_elementOffsets = _serde::__private::Some(
                                    match __A::next_value::<[I32<'de>; 32usize]>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_memory) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("memory"),
                                    );
                                }
                                __A::pad(&mut __map, 0usize, 4usize)?;
                                m_memory = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_vertexStride) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vertexStride",
                                        ),
                                    );
                                }
                                m_vertexStride = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_locked) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("locked"),
                                    );
                                }
                                m_locked = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(&m_numVertices) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numVertices",
                                        ),
                                    );
                                }
                                __A::pad(&mut __map, 3usize, 3usize)?;
                                m_numVertices = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(&m_isBigEndian) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "isBigEndian",
                                        ),
                                    );
                                }
                                m_isBigEndian = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(&m_isSharable) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "isSharable",
                                        ),
                                    );
                                }
                                m_isSharable = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
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
                    __A::pad(&mut __map, 2usize, 2usize)?;
                    let m_format = match m_format {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("format"),
                            );
                        }
                    };
                    let m_elementOffsets = match m_elementOffsets {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elementOffsets",
                                ),
                            );
                        }
                    };
                    let m_memory = match m_memory {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("memory"),
                            );
                        }
                    };
                    let m_vertexStride = match m_vertexStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vertexStride",
                                ),
                            );
                        }
                    };
                    let m_locked = match m_locked {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("locked"),
                            );
                        }
                    };
                    let m_numVertices = match m_numVertices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numVertices",
                                ),
                            );
                        }
                    };
                    let m_isBigEndian = match m_isBigEndian {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "isBigEndian",
                                ),
                            );
                        }
                    };
                    let m_isSharable = match m_isSharable {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "isSharable",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkMemoryMeshVertexBuffer {
                        __ptr,
                        parent,
                        m_format,
                        m_elementOffsets,
                        m_memory,
                        m_vertexStride,
                        m_locked,
                        m_numVertices,
                        m_isBigEndian,
                        m_isSharable,
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
                    let mut m_format: _serde::__private::Option<hkVertexFormat<'de>> = _serde::__private::None;
                    let mut m_elementOffsets: _serde::__private::Option<
                        [I32<'de>; 32usize],
                    > = _serde::__private::None;
                    let mut m_memory: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_vertexStride: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_locked: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_numVertices: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_isBigEndian: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_isSharable: _serde::__private::Option<bool> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_format => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_format) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("format"),
                                    );
                                }
                                m_format = _serde::__private::Some(
                                    match __A::next_value::<hkVertexFormat<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_elementOffsets => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_elementOffsets) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elementOffsets",
                                        ),
                                    );
                                }
                                m_elementOffsets = _serde::__private::Some(
                                    match __A::next_value::<[I32<'de>; 32usize]>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_memory => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_memory) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("memory"),
                                    );
                                }
                                m_memory = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_vertexStride => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_vertexStride) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vertexStride",
                                        ),
                                    );
                                }
                                m_vertexStride = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_locked => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_locked) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("locked"),
                                    );
                                }
                                m_locked = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_numVertices => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_numVertices) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numVertices",
                                        ),
                                    );
                                }
                                m_numVertices = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_isBigEndian => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_isBigEndian) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "isBigEndian",
                                        ),
                                    );
                                }
                                m_isBigEndian = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_isSharable => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_isSharable) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "isSharable",
                                        ),
                                    );
                                }
                                m_isSharable = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
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
                    let m_format = match m_format {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("format"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_elementOffsets = match m_elementOffsets {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elementOffsets",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_memory = match m_memory {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("memory"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_vertexStride = match m_vertexStride {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "vertexStride",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_locked = match m_locked {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("locked"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_numVertices = match m_numVertices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numVertices",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_isBigEndian = match m_isBigEndian {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "isBigEndian",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_isSharable = match m_isSharable {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "isSharable",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = None;
                    let parent = hkBaseObject {
                        __ptr: __ptr.clone(),
                    };
                    let parent = hkReferencedObject {
                        __ptr: __ptr.clone(),
                        parent,
                        ..Default::default()
                    };
                    let parent = hkMeshVertexBuffer {
                        __ptr: __ptr.clone(),
                        parent,
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkMemoryMeshVertexBuffer {
                        __ptr: __ptr.clone(),
                        parent,
                        m_format,
                        m_elementOffsets,
                        m_memory,
                        m_vertexStride,
                        m_locked,
                        m_numVertices,
                        m_isBigEndian,
                        m_isSharable,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "format",
                "elementOffsets",
                "memory",
                "vertexStride",
                "locked",
                "numVertices",
                "isBigEndian",
                "isSharable",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkMemoryMeshVertexBuffer",
                FIELDS,
                __hkMemoryMeshVertexBufferVisitor {
                    marker: _serde::__private::PhantomData::<hkMemoryMeshVertexBuffer>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
