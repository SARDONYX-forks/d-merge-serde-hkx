use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbRigidBodyRagdollControlData`
/// - version: `1`
/// - signature: `0x1e0bc068`
/// - size: ` 64`(x86)/` 64`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbRigidBodyRagdollControlData<'a> {
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
    /// - name: `keyFrameHierarchyControlData`(ctype: `struct hkaKeyFrameHierarchyUtilityControlData`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: ` 48`(x86)/` 48`(x86_64)
    /// - flags: `ALIGN_16`
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(
        feature = "json_schema",
        schemars(rename = "keyFrameHierarchyControlData")
    )]
    #[cfg_attr(feature = "serde", serde(rename = "keyFrameHierarchyControlData"))]
    pub m_keyFrameHierarchyControlData: hkaKeyFrameHierarchyUtilityControlData<'a>,
    /// # C++ Info
    /// - name: `durationToBlend`(ctype: `hkReal`)
    /// - offset: ` 48`(x86)/` 48`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "durationToBlend"))]
    #[cfg_attr(feature = "serde", serde(rename = "durationToBlend"))]
    pub m_durationToBlend: f32,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkbRigidBodyRagdollControlData<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbRigidBodyRagdollControlData"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0x1e0bc068)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_keyFrameHierarchyControlData.deps_indexes());
            v
        }
    }
    impl<'a> _serde::Serialize for hkbRigidBodyRagdollControlData<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0x1e0bc068)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkbRigidBodyRagdollControlData",
                    class_meta,
                    (64u64, 64u64),
                )?;
            serializer
                .serialize_field(
                    "keyFrameHierarchyControlData",
                    &self.m_keyFrameHierarchyControlData,
                )?;
            serializer.serialize_field("durationToBlend", &self.m_durationToBlend)?;
            serializer.pad_field([0u8; 12usize].as_slice(), [0u8; 12usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkbRigidBodyRagdollControlData<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_keyFrameHierarchyControlData,
                m_durationToBlend,
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
                        "keyFrameHierarchyControlData" => {
                            Ok(__Field::m_keyFrameHierarchyControlData)
                        }
                        "durationToBlend" => Ok(__Field::m_durationToBlend),
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
            struct __hkbRigidBodyRagdollControlDataVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkbRigidBodyRagdollControlData<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkbRigidBodyRagdollControlDataVisitor<'de> {
                type Value = hkbRigidBodyRagdollControlData<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbRigidBodyRagdollControlData",
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
                    let mut m_keyFrameHierarchyControlData: _serde::__private::Option<
                        hkaKeyFrameHierarchyUtilityControlData,
                    > = _serde::__private::None;
                    let mut m_durationToBlend: _serde::__private::Option<f32> = _serde::__private::None;
                    for i in 0..2usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(
                                    &m_keyFrameHierarchyControlData,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "keyFrameHierarchyControlData",
                                        ),
                                    );
                                }
                                m_keyFrameHierarchyControlData = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkaKeyFrameHierarchyUtilityControlData,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_durationToBlend) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "durationToBlend",
                                        ),
                                    );
                                }
                                m_durationToBlend = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
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
                    __A::pad(&mut __map, 12usize, 12usize)?;
                    let m_keyFrameHierarchyControlData = match m_keyFrameHierarchyControlData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "keyFrameHierarchyControlData",
                                ),
                            );
                        }
                    };
                    let m_durationToBlend = match m_durationToBlend {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "durationToBlend",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbRigidBodyRagdollControlData {
                        __ptr,
                        m_keyFrameHierarchyControlData,
                        m_durationToBlend,
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
                    let mut m_keyFrameHierarchyControlData: _serde::__private::Option<
                        hkaKeyFrameHierarchyUtilityControlData,
                    > = _serde::__private::None;
                    let mut m_durationToBlend: _serde::__private::Option<f32> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_keyFrameHierarchyControlData => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_keyFrameHierarchyControlData,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "keyFrameHierarchyControlData",
                                        ),
                                    );
                                }
                                m_keyFrameHierarchyControlData = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkaKeyFrameHierarchyUtilityControlData,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_durationToBlend => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_durationToBlend) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "durationToBlend",
                                        ),
                                    );
                                }
                                m_durationToBlend = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
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
                    let m_keyFrameHierarchyControlData = match m_keyFrameHierarchyControlData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "keyFrameHierarchyControlData",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_durationToBlend = match m_durationToBlend {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "durationToBlend",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkbRigidBodyRagdollControlData {
                        __ptr: __ptr.clone(),
                        m_keyFrameHierarchyControlData,
                        m_durationToBlend,
                    })
                }
            }
            const FIELDS: &[&str] = &["keyFrameHierarchyControlData", "durationToBlend"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbRigidBodyRagdollControlData",
                FIELDS,
                __hkbRigidBodyRagdollControlDataVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkbRigidBodyRagdollControlData,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
