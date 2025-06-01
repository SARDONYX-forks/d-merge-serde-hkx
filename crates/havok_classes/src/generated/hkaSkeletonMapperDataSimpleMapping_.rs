use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkaSkeletonMapperDataSimpleMapping`
/// - version: `0`
/// - signature: `0x3405deca`
/// - size: ` 64`(x86)/` 64`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkaSkeletonMapperDataSimpleMapping<'a> {
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
    /// - name: `boneA`(ctype: `hkInt16`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "boneA"))]
    #[cfg_attr(feature = "serde", serde(rename = "boneA"))]
    pub m_boneA: I16<'a>,
    /// # C++ Info
    /// - name: `boneB`(ctype: `hkInt16`)
    /// - offset: `  2`(x86)/`  2`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "boneB"))]
    #[cfg_attr(feature = "serde", serde(rename = "boneB"))]
    pub m_boneB: I16<'a>,
    /// # C++ Info
    /// - name: `aFromBTransform`(ctype: `hkQsTransform`)
    /// - offset: ` 16`(x86)/` 16`(x86_64)
    /// - type_size: ` 48`(x86)/` 48`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "aFromBTransform"))]
    #[cfg_attr(feature = "serde", serde(rename = "aFromBTransform"))]
    pub m_aFromBTransform: QsTransform,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkaSkeletonMapperDataSimpleMapping<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkaSkeletonMapperDataSimpleMapping"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0x3405deca)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v
        }
    }
    impl<'a> _serde::Serialize for hkaSkeletonMapperDataSimpleMapping<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0x3405deca)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkaSkeletonMapperDataSimpleMapping",
                    class_meta,
                    (64u64, 64u64),
                )?;
            serializer.serialize_field("boneA", &self.m_boneA)?;
            serializer.serialize_field("boneB", &self.m_boneB)?;
            serializer.pad_field([0u8; 12usize].as_slice(), [0u8; 12usize].as_slice())?;
            serializer.serialize_field("aFromBTransform", &self.m_aFromBTransform)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkaSkeletonMapperDataSimpleMapping<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_boneA,
                m_boneB,
                m_aFromBTransform,
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
                        "boneA" => Ok(__Field::m_boneA),
                        "boneB" => Ok(__Field::m_boneB),
                        "aFromBTransform" => Ok(__Field::m_aFromBTransform),
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
            struct __hkaSkeletonMapperDataSimpleMappingVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkaSkeletonMapperDataSimpleMapping<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkaSkeletonMapperDataSimpleMappingVisitor<'de> {
                type Value = hkaSkeletonMapperDataSimpleMapping<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkaSkeletonMapperDataSimpleMapping",
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
                    let mut m_boneA: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_boneB: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_aFromBTransform: _serde::__private::Option<QsTransform> = _serde::__private::None;
                    for i in 0..3usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_boneA) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("boneA"),
                                    );
                                }
                                m_boneA = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_boneB) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("boneB"),
                                    );
                                }
                                m_boneB = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_aFromBTransform) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "aFromBTransform",
                                        ),
                                    );
                                }
                                __A::pad(&mut __map, 12usize, 12usize)?;
                                m_aFromBTransform = _serde::__private::Some(
                                    match __A::next_value::<QsTransform>(&mut __map) {
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
                    let m_boneA = match m_boneA {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("boneA"),
                            );
                        }
                    };
                    let m_boneB = match m_boneB {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("boneB"),
                            );
                        }
                    };
                    let m_aFromBTransform = match m_aFromBTransform {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "aFromBTransform",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkaSkeletonMapperDataSimpleMapping {
                        __ptr,
                        m_boneA,
                        m_boneB,
                        m_aFromBTransform,
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
                    let mut m_boneA: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_boneB: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_aFromBTransform: _serde::__private::Option<QsTransform> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_boneA => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_boneA) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("boneA"),
                                    );
                                }
                                m_boneA = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_boneB => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_boneB) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("boneB"),
                                    );
                                }
                                m_boneB = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_aFromBTransform => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_aFromBTransform) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "aFromBTransform",
                                        ),
                                    );
                                }
                                m_aFromBTransform = _serde::__private::Some(
                                    match __A::next_value::<QsTransform>(&mut __map) {
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
                    let m_boneA = match m_boneA {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("boneA"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_boneB = match m_boneB {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("boneB"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_aFromBTransform = match m_aFromBTransform {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "aFromBTransform",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkaSkeletonMapperDataSimpleMapping {
                        __ptr: __ptr.clone(),
                        m_boneA,
                        m_boneB,
                        m_aFromBTransform,
                    })
                }
            }
            const FIELDS: &[&str] = &["boneA", "boneB", "aFromBTransform"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkaSkeletonMapperDataSimpleMapping",
                FIELDS,
                __hkaSkeletonMapperDataSimpleMappingVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkaSkeletonMapperDataSimpleMapping,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
