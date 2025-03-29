use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpStorageMeshShapeSubpartStorage`
/// - version: `0`
/// - signature: `0xbf27438`
/// - size: ` 80`(x86)/`112`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpStorageMeshShapeSubpartStorage<'a> {
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
    pub parent: hkReferencedObject<'a>,
    /// # C++ Info
    /// - name: `vertices`(ctype: `hkArray<hkReal>`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "vertices"))]
    #[cfg_attr(feature = "serde", serde(rename = "vertices"))]
    pub m_vertices: Vec<f32>,
    /// # C++ Info
    /// - name: `indices16`(ctype: `hkArray<hkUint16>`)
    /// - offset: ` 20`(x86)/` 32`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "indices16"))]
    #[cfg_attr(feature = "serde", serde(rename = "indices16"))]
    pub m_indices16: Vec<U16<'a>>,
    /// # C++ Info
    /// - name: `indices32`(ctype: `hkArray<hkUint32>`)
    /// - offset: ` 32`(x86)/` 48`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "indices32"))]
    #[cfg_attr(feature = "serde", serde(rename = "indices32"))]
    pub m_indices32: Vec<U32<'a>>,
    /// # C++ Info
    /// - name: `materialIndices`(ctype: `hkArray<hkUint8>`)
    /// - offset: ` 44`(x86)/` 64`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "materialIndices"))]
    #[cfg_attr(feature = "serde", serde(rename = "materialIndices"))]
    pub m_materialIndices: Vec<U8<'a>>,
    /// # C++ Info
    /// - name: `materials`(ctype: `hkArray<hkUint32>`)
    /// - offset: ` 56`(x86)/` 80`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "materials"))]
    #[cfg_attr(feature = "serde", serde(rename = "materials"))]
    pub m_materials: Vec<U32<'a>>,
    /// # C++ Info
    /// - name: `materialIndices16`(ctype: `hkArray<hkUint16>`)
    /// - offset: ` 68`(x86)/` 96`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "materialIndices16"))]
    #[cfg_attr(feature = "serde", serde(rename = "materialIndices16"))]
    pub m_materialIndices16: Vec<U16<'a>>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpStorageMeshShapeSubpartStorage<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpStorageMeshShapeSubpartStorage"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xbf27438)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v
        }
    }
    impl<'a> _serde::Serialize for hkpStorageMeshShapeSubpartStorage<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xbf27438)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkpStorageMeshShapeSubpartStorage",
                    class_meta,
                    (80u64, 112u64),
                )?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer.skip_field("memSizeAndFlags", &self.parent.m_memSizeAndFlags)?;
            serializer.skip_field("referenceCount", &self.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer
                .serialize_array_field("vertices", &self.m_vertices, TypeSize::NonPtr)?;
            serializer
                .serialize_array_field(
                    "indices16",
                    &self.m_indices16,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "indices32",
                    &self.m_indices32,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "materialIndices",
                    &self.m_materialIndices,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "materials",
                    &self.m_materials,
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_array_field(
                    "materialIndices16",
                    &self.m_materialIndices16,
                    TypeSize::NonPtr,
                )?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkpStorageMeshShapeSubpartStorage<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_vertices,
                m_indices16,
                m_indices32,
                m_materialIndices,
                m_materials,
                m_materialIndices16,
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
                        "vertices" => Ok(__Field::m_vertices),
                        "indices16" => Ok(__Field::m_indices16),
                        "indices32" => Ok(__Field::m_indices32),
                        "materialIndices" => Ok(__Field::m_materialIndices),
                        "materials" => Ok(__Field::m_materials),
                        "materialIndices16" => Ok(__Field::m_materialIndices16),
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
            struct __hkpStorageMeshShapeSubpartStorageVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkpStorageMeshShapeSubpartStorage<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkpStorageMeshShapeSubpartStorageVisitor<'de> {
                type Value = hkpStorageMeshShapeSubpartStorage<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkpStorageMeshShapeSubpartStorage",
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
                    let mut m_vertices: _serde::__private::Option<Vec<f32>> = _serde::__private::None;
                    let mut m_indices16: _serde::__private::Option<Vec<U16<'de>>> = _serde::__private::None;
                    let mut m_indices32: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_materialIndices: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_materials: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_materialIndices16: _serde::__private::Option<
                        Vec<U16<'de>>,
                    > = _serde::__private::None;
                    for i in 0..6usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_vertices) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vertices",
                                        ),
                                    );
                                }
                                m_vertices = _serde::__private::Some(
                                    match __A::next_value::<Vec<f32>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_indices16) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "indices16",
                                        ),
                                    );
                                }
                                m_indices16 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_indices32) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "indices32",
                                        ),
                                    );
                                }
                                m_indices32 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_materialIndices) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materialIndices",
                                        ),
                                    );
                                }
                                m_materialIndices = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_materials) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materials",
                                        ),
                                    );
                                }
                                m_materials = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(
                                    &m_materialIndices16,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materialIndices16",
                                        ),
                                    );
                                }
                                m_materialIndices16 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
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
                    let m_vertices = match m_vertices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("vertices"),
                            );
                        }
                    };
                    let m_indices16 = match m_indices16 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "indices16",
                                ),
                            );
                        }
                    };
                    let m_indices32 = match m_indices32 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "indices32",
                                ),
                            );
                        }
                    };
                    let m_materialIndices = match m_materialIndices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materialIndices",
                                ),
                            );
                        }
                    };
                    let m_materials = match m_materials {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materials",
                                ),
                            );
                        }
                    };
                    let m_materialIndices16 = match m_materialIndices16 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materialIndices16",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkpStorageMeshShapeSubpartStorage {
                        __ptr,
                        parent,
                        m_vertices,
                        m_indices16,
                        m_indices32,
                        m_materialIndices,
                        m_materials,
                        m_materialIndices16,
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
                    let mut m_vertices: _serde::__private::Option<Vec<f32>> = _serde::__private::None;
                    let mut m_indices16: _serde::__private::Option<Vec<U16<'de>>> = _serde::__private::None;
                    let mut m_indices32: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_materialIndices: _serde::__private::Option<Vec<U8<'de>>> = _serde::__private::None;
                    let mut m_materials: _serde::__private::Option<Vec<U32<'de>>> = _serde::__private::None;
                    let mut m_materialIndices16: _serde::__private::Option<
                        Vec<U16<'de>>,
                    > = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_vertices => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_vertices) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "vertices",
                                        ),
                                    );
                                }
                                m_vertices = _serde::__private::Some(
                                    match __A::next_value::<Vec<f32>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_indices16 => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_indices16) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "indices16",
                                        ),
                                    );
                                }
                                m_indices16 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_indices32 => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_indices32) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "indices32",
                                        ),
                                    );
                                }
                                m_indices32 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_materialIndices => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_materialIndices) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materialIndices",
                                        ),
                                    );
                                }
                                m_materialIndices = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_materials => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_materials) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materials",
                                        ),
                                    );
                                }
                                m_materials = _serde::__private::Some(
                                    match __A::next_value::<Vec<U32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_materialIndices16 => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_materialIndices16,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "materialIndices16",
                                        ),
                                    );
                                }
                                m_materialIndices16 = _serde::__private::Some(
                                    match __A::next_value::<Vec<U16<'de>>>(&mut __map) {
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
                    let m_vertices = match m_vertices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("vertices"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_indices16 = match m_indices16 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "indices16",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_indices32 = match m_indices32 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "indices32",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_materialIndices = match m_materialIndices {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materialIndices",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_materials = match m_materials {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materials",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_materialIndices16 = match m_materialIndices16 {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "materialIndices16",
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
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpStorageMeshShapeSubpartStorage {
                        __ptr: __ptr.clone(),
                        parent,
                        m_vertices,
                        m_indices16,
                        m_indices32,
                        m_materialIndices,
                        m_materials,
                        m_materialIndices16,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "vertices",
                "indices16",
                "indices32",
                "materialIndices",
                "materials",
                "materialIndices16",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpStorageMeshShapeSubpartStorage",
                FIELDS,
                __hkpStorageMeshShapeSubpartStorageVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkpStorageMeshShapeSubpartStorage,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
