use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpEntityExtendedListeners`
/// - version: `0`
/// - signature: `0xf557023c`
/// - size: ` 16`(x86)/` 32`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpEntityExtendedListeners<'a> {
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
    /// - name: `activationListeners`(ctype: `struct hkpEntitySmallArraySerializeOverrideType`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "activationListeners"))]
    #[cfg_attr(feature = "serde", serde(rename = "activationListeners"))]
    pub m_activationListeners: hkpEntitySmallArraySerializeOverrideType<'a>,
    /// # C++ Info
    /// - name: `entityListeners`(ctype: `struct hkpEntitySmallArraySerializeOverrideType`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "entityListeners"))]
    #[cfg_attr(feature = "serde", serde(rename = "entityListeners"))]
    pub m_entityListeners: hkpEntitySmallArraySerializeOverrideType<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpEntityExtendedListeners<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpEntityExtendedListeners"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xf557023c)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_activationListeners.deps_indexes());
            v.extend(self.m_entityListeners.deps_indexes());
            v
        }
    }
    impl<'a> _serde::Serialize for hkpEntityExtendedListeners<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xf557023c)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkpEntityExtendedListeners",
                    class_meta,
                    (16u64, 32u64),
                )?;
            serializer.skip_field("activationListeners", &self.m_activationListeners)?;
            serializer.skip_field("entityListeners", &self.m_entityListeners)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkpEntityExtendedListeners<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
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
            struct __hkpEntityExtendedListenersVisitor<'de> {
                marker: _serde::__private::PhantomData<hkpEntityExtendedListeners<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkpEntityExtendedListenersVisitor<'de> {
                type Value = hkpEntityExtendedListeners<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkpEntityExtendedListeners",
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
                    let mut m_activationListeners: _serde::__private::Option<
                        hkpEntitySmallArraySerializeOverrideType<'de>,
                    > = _serde::__private::None;
                    let mut m_entityListeners: _serde::__private::Option<
                        hkpEntitySmallArraySerializeOverrideType<'de>,
                    > = _serde::__private::None;
                    for i in 0..2usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(
                                    &m_activationListeners,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "activationListeners",
                                        ),
                                    );
                                }
                                m_activationListeners = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpEntitySmallArraySerializeOverrideType<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_entityListeners) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "entityListeners",
                                        ),
                                    );
                                }
                                m_entityListeners = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpEntitySmallArraySerializeOverrideType<'de>,
                                    >(&mut __map) {
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
                    let m_activationListeners = match m_activationListeners {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "activationListeners",
                                ),
                            );
                        }
                    };
                    let m_entityListeners = match m_entityListeners {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "entityListeners",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkpEntityExtendedListeners {
                        __ptr,
                        m_activationListeners,
                        m_entityListeners,
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
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            _ => __A::skip_value(&mut __map)?,
                        }
                    }
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpEntityExtendedListeners {
                        __ptr: __ptr.clone(),
                        ..Default::default()
                    })
                }
            }
            const FIELDS: &[&str] = &["activationListeners", "entityListeners"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpEntityExtendedListeners",
                FIELDS,
                __hkpEntityExtendedListenersVisitor {
                    marker: _serde::__private::PhantomData::<hkpEntityExtendedListeners>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
