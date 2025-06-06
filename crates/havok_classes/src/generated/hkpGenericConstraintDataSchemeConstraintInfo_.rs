use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpGenericConstraintDataSchemeConstraintInfo`
/// - version: `0`
/// - signature: `0xd6421f19`
/// - size: ` 16`(x86)/` 16`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpGenericConstraintDataSchemeConstraintInfo<'a> {
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
    /// - name: `maxSizeOfSchema`(ctype: `hkInt32`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "maxSizeOfSchema"))]
    #[cfg_attr(feature = "serde", serde(rename = "maxSizeOfSchema"))]
    pub m_maxSizeOfSchema: I32<'a>,
    /// # C++ Info
    /// - name: `sizeOfSchemas`(ctype: `hkInt32`)
    /// - offset: `  4`(x86)/`  4`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "sizeOfSchemas"))]
    #[cfg_attr(feature = "serde", serde(rename = "sizeOfSchemas"))]
    pub m_sizeOfSchemas: I32<'a>,
    /// # C++ Info
    /// - name: `numSolverResults`(ctype: `hkInt32`)
    /// - offset: `  8`(x86)/`  8`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "numSolverResults"))]
    #[cfg_attr(feature = "serde", serde(rename = "numSolverResults"))]
    pub m_numSolverResults: I32<'a>,
    /// # C++ Info
    /// - name: `numSolverElemTemps`(ctype: `hkInt32`)
    /// - offset: ` 12`(x86)/` 12`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "numSolverElemTemps"))]
    #[cfg_attr(feature = "serde", serde(rename = "numSolverElemTemps"))]
    pub m_numSolverElemTemps: I32<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpGenericConstraintDataSchemeConstraintInfo<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpGenericConstraintDataSchemeConstraintInfo"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xd6421f19)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v
        }
    }
    impl<'a> _serde::Serialize for hkpGenericConstraintDataSchemeConstraintInfo<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xd6421f19)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkpGenericConstraintDataSchemeConstraintInfo",
                    class_meta,
                    (16u64, 16u64),
                )?;
            serializer.serialize_field("maxSizeOfSchema", &self.m_maxSizeOfSchema)?;
            serializer.serialize_field("sizeOfSchemas", &self.m_sizeOfSchemas)?;
            serializer.serialize_field("numSolverResults", &self.m_numSolverResults)?;
            serializer
                .serialize_field("numSolverElemTemps", &self.m_numSolverElemTemps)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de>
    for hkpGenericConstraintDataSchemeConstraintInfo<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_maxSizeOfSchema,
                m_sizeOfSchemas,
                m_numSolverResults,
                m_numSolverElemTemps,
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
                        "maxSizeOfSchema" => Ok(__Field::m_maxSizeOfSchema),
                        "sizeOfSchemas" => Ok(__Field::m_sizeOfSchemas),
                        "numSolverResults" => Ok(__Field::m_numSolverResults),
                        "numSolverElemTemps" => Ok(__Field::m_numSolverElemTemps),
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
            struct __hkpGenericConstraintDataSchemeConstraintInfoVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkpGenericConstraintDataSchemeConstraintInfo<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkpGenericConstraintDataSchemeConstraintInfoVisitor<'de> {
                type Value = hkpGenericConstraintDataSchemeConstraintInfo<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkpGenericConstraintDataSchemeConstraintInfo",
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
                    let mut m_maxSizeOfSchema: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_sizeOfSchemas: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_numSolverResults: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_numSolverElemTemps: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    for i in 0..4usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_maxSizeOfSchema) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "maxSizeOfSchema",
                                        ),
                                    );
                                }
                                m_maxSizeOfSchema = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_sizeOfSchemas) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "sizeOfSchemas",
                                        ),
                                    );
                                }
                                m_sizeOfSchemas = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_numSolverResults) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numSolverResults",
                                        ),
                                    );
                                }
                                m_numSolverResults = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(
                                    &m_numSolverElemTemps,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numSolverElemTemps",
                                        ),
                                    );
                                }
                                m_numSolverElemTemps = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
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
                    let m_maxSizeOfSchema = match m_maxSizeOfSchema {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "maxSizeOfSchema",
                                ),
                            );
                        }
                    };
                    let m_sizeOfSchemas = match m_sizeOfSchemas {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "sizeOfSchemas",
                                ),
                            );
                        }
                    };
                    let m_numSolverResults = match m_numSolverResults {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numSolverResults",
                                ),
                            );
                        }
                    };
                    let m_numSolverElemTemps = match m_numSolverElemTemps {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numSolverElemTemps",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkpGenericConstraintDataSchemeConstraintInfo {
                        __ptr,
                        m_maxSizeOfSchema,
                        m_sizeOfSchemas,
                        m_numSolverResults,
                        m_numSolverElemTemps,
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
                    let mut m_maxSizeOfSchema: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_sizeOfSchemas: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_numSolverResults: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_numSolverElemTemps: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_maxSizeOfSchema => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_maxSizeOfSchema) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "maxSizeOfSchema",
                                        ),
                                    );
                                }
                                m_maxSizeOfSchema = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_sizeOfSchemas => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_sizeOfSchemas) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "sizeOfSchemas",
                                        ),
                                    );
                                }
                                m_sizeOfSchemas = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_numSolverResults => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_numSolverResults) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numSolverResults",
                                        ),
                                    );
                                }
                                m_numSolverResults = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_numSolverElemTemps => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_numSolverElemTemps,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numSolverElemTemps",
                                        ),
                                    );
                                }
                                m_numSolverElemTemps = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
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
                    let m_maxSizeOfSchema = match m_maxSizeOfSchema {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "maxSizeOfSchema",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_sizeOfSchemas = match m_sizeOfSchemas {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "sizeOfSchemas",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_numSolverResults = match m_numSolverResults {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numSolverResults",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_numSolverElemTemps = match m_numSolverElemTemps {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numSolverElemTemps",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpGenericConstraintDataSchemeConstraintInfo {
                        __ptr: __ptr.clone(),
                        m_maxSizeOfSchema,
                        m_sizeOfSchemas,
                        m_numSolverResults,
                        m_numSolverElemTemps,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "maxSizeOfSchema",
                "sizeOfSchemas",
                "numSolverResults",
                "numSolverElemTemps",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpGenericConstraintDataSchemeConstraintInfo",
                FIELDS,
                __hkpGenericConstraintDataSchemeConstraintInfoVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkpGenericConstraintDataSchemeConstraintInfo,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
