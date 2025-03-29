use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbHandIkDriverInfoHand`
/// - version: `1`
/// - signature: `0x14dfe1dd`
/// - size: ` 96`(x86)/` 96`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbHandIkDriverInfoHand<'a> {
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
    /// - name: `elbowAxisLS`(ctype: `hkVector4`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "elbowAxisLS"))]
    #[cfg_attr(feature = "serde", serde(rename = "elbowAxisLS"))]
    pub m_elbowAxisLS: Vector4,
    /// # C++ Info
    /// - name: `backHandNormalLS`(ctype: `hkVector4`)
    /// - offset: ` 16`(x86)/` 16`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "backHandNormalLS"))]
    #[cfg_attr(feature = "serde", serde(rename = "backHandNormalLS"))]
    pub m_backHandNormalLS: Vector4,
    /// # C++ Info
    /// - name: `handOffsetLS`(ctype: `hkVector4`)
    /// - offset: ` 32`(x86)/` 32`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "handOffsetLS"))]
    #[cfg_attr(feature = "serde", serde(rename = "handOffsetLS"))]
    pub m_handOffsetLS: Vector4,
    /// # C++ Info
    /// - name: `handOrienationOffsetLS`(ctype: `hkQuaternion`)
    /// - offset: ` 48`(x86)/` 48`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "handOrienationOffsetLS"))]
    #[cfg_attr(feature = "serde", serde(rename = "handOrienationOffsetLS"))]
    pub m_handOrienationOffsetLS: Quaternion,
    /// # C++ Info
    /// - name: `maxElbowAngleDegrees`(ctype: `hkReal`)
    /// - offset: ` 64`(x86)/` 64`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "maxElbowAngleDegrees"))]
    #[cfg_attr(feature = "serde", serde(rename = "maxElbowAngleDegrees"))]
    pub m_maxElbowAngleDegrees: f32,
    /// # C++ Info
    /// - name: `minElbowAngleDegrees`(ctype: `hkReal`)
    /// - offset: ` 68`(x86)/` 68`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "minElbowAngleDegrees"))]
    #[cfg_attr(feature = "serde", serde(rename = "minElbowAngleDegrees"))]
    pub m_minElbowAngleDegrees: f32,
    /// # C++ Info
    /// - name: `shoulderIndex`(ctype: `hkInt16`)
    /// - offset: ` 72`(x86)/` 72`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "shoulderIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "shoulderIndex"))]
    pub m_shoulderIndex: I16<'a>,
    /// # C++ Info
    /// - name: `shoulderSiblingIndex`(ctype: `hkInt16`)
    /// - offset: ` 74`(x86)/` 74`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "shoulderSiblingIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "shoulderSiblingIndex"))]
    pub m_shoulderSiblingIndex: I16<'a>,
    /// # C++ Info
    /// - name: `elbowIndex`(ctype: `hkInt16`)
    /// - offset: ` 76`(x86)/` 76`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "elbowIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "elbowIndex"))]
    pub m_elbowIndex: I16<'a>,
    /// # C++ Info
    /// - name: `elbowSiblingIndex`(ctype: `hkInt16`)
    /// - offset: ` 78`(x86)/` 78`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "elbowSiblingIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "elbowSiblingIndex"))]
    pub m_elbowSiblingIndex: I16<'a>,
    /// # C++ Info
    /// - name: `wristIndex`(ctype: `hkInt16`)
    /// - offset: ` 80`(x86)/` 80`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "wristIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "wristIndex"))]
    pub m_wristIndex: I16<'a>,
    /// # C++ Info
    /// - name: `enforceEndPosition`(ctype: `hkBool`)
    /// - offset: ` 82`(x86)/` 82`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "enforceEndPosition"))]
    #[cfg_attr(feature = "serde", serde(rename = "enforceEndPosition"))]
    pub m_enforceEndPosition: bool,
    /// # C++ Info
    /// - name: `enforceEndRotation`(ctype: `hkBool`)
    /// - offset: ` 83`(x86)/` 83`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "enforceEndRotation"))]
    #[cfg_attr(feature = "serde", serde(rename = "enforceEndRotation"))]
    pub m_enforceEndRotation: bool,
    /// # C++ Info
    /// - name: `localFrameName`(ctype: `hkStringPtr`)
    /// - offset: ` 84`(x86)/` 88`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "localFrameName"))]
    #[cfg_attr(feature = "serde", serde(rename = "localFrameName"))]
    pub m_localFrameName: StringPtr<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkbHandIkDriverInfoHand<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbHandIkDriverInfoHand"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0x14dfe1dd)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v
        }
    }
    impl<'a> _serde::Serialize for hkbHandIkDriverInfoHand<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0x14dfe1dd)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkbHandIkDriverInfoHand",
                    class_meta,
                    (96u64, 96u64),
                )?;
            serializer.serialize_field("elbowAxisLS", &self.m_elbowAxisLS)?;
            serializer.serialize_field("backHandNormalLS", &self.m_backHandNormalLS)?;
            serializer.serialize_field("handOffsetLS", &self.m_handOffsetLS)?;
            serializer
                .serialize_field(
                    "handOrienationOffsetLS",
                    &self.m_handOrienationOffsetLS,
                )?;
            serializer
                .serialize_field("maxElbowAngleDegrees", &self.m_maxElbowAngleDegrees)?;
            serializer
                .serialize_field("minElbowAngleDegrees", &self.m_minElbowAngleDegrees)?;
            serializer.serialize_field("shoulderIndex", &self.m_shoulderIndex)?;
            serializer
                .serialize_field("shoulderSiblingIndex", &self.m_shoulderSiblingIndex)?;
            serializer.serialize_field("elbowIndex", &self.m_elbowIndex)?;
            serializer.serialize_field("elbowSiblingIndex", &self.m_elbowSiblingIndex)?;
            serializer.serialize_field("wristIndex", &self.m_wristIndex)?;
            serializer
                .serialize_field("enforceEndPosition", &self.m_enforceEndPosition)?;
            serializer
                .serialize_field("enforceEndRotation", &self.m_enforceEndRotation)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.serialize_field("localFrameName", &self.m_localFrameName)?;
            serializer.pad_field([0u8; 8usize].as_slice(), [0u8; 0usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkbHandIkDriverInfoHand<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_elbowAxisLS,
                m_backHandNormalLS,
                m_handOffsetLS,
                m_handOrienationOffsetLS,
                m_maxElbowAngleDegrees,
                m_minElbowAngleDegrees,
                m_shoulderIndex,
                m_shoulderSiblingIndex,
                m_elbowIndex,
                m_elbowSiblingIndex,
                m_wristIndex,
                m_enforceEndPosition,
                m_enforceEndRotation,
                m_localFrameName,
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
                        "elbowAxisLS" => Ok(__Field::m_elbowAxisLS),
                        "backHandNormalLS" => Ok(__Field::m_backHandNormalLS),
                        "handOffsetLS" => Ok(__Field::m_handOffsetLS),
                        "handOrienationOffsetLS" => Ok(__Field::m_handOrienationOffsetLS),
                        "maxElbowAngleDegrees" => Ok(__Field::m_maxElbowAngleDegrees),
                        "minElbowAngleDegrees" => Ok(__Field::m_minElbowAngleDegrees),
                        "shoulderIndex" => Ok(__Field::m_shoulderIndex),
                        "shoulderSiblingIndex" => Ok(__Field::m_shoulderSiblingIndex),
                        "elbowIndex" => Ok(__Field::m_elbowIndex),
                        "elbowSiblingIndex" => Ok(__Field::m_elbowSiblingIndex),
                        "wristIndex" => Ok(__Field::m_wristIndex),
                        "enforceEndPosition" => Ok(__Field::m_enforceEndPosition),
                        "enforceEndRotation" => Ok(__Field::m_enforceEndRotation),
                        "localFrameName" => Ok(__Field::m_localFrameName),
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
            struct __hkbHandIkDriverInfoHandVisitor<'de> {
                marker: _serde::__private::PhantomData<hkbHandIkDriverInfoHand<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkbHandIkDriverInfoHandVisitor<'de> {
                type Value = hkbHandIkDriverInfoHand<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbHandIkDriverInfoHand",
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
                    let mut m_elbowAxisLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_backHandNormalLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_handOffsetLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_handOrienationOffsetLS: _serde::__private::Option<
                        Quaternion,
                    > = _serde::__private::None;
                    let mut m_maxElbowAngleDegrees: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_minElbowAngleDegrees: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_shoulderIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_shoulderSiblingIndex: _serde::__private::Option<
                        I16<'de>,
                    > = _serde::__private::None;
                    let mut m_elbowIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_elbowSiblingIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_wristIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_enforceEndPosition: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_enforceEndRotation: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_localFrameName: _serde::__private::Option<
                        StringPtr<'de>,
                    > = _serde::__private::None;
                    for i in 0..14usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_elbowAxisLS) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowAxisLS",
                                        ),
                                    );
                                }
                                m_elbowAxisLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_backHandNormalLS) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "backHandNormalLS",
                                        ),
                                    );
                                }
                                m_backHandNormalLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_handOffsetLS) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "handOffsetLS",
                                        ),
                                    );
                                }
                                m_handOffsetLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(
                                    &m_handOrienationOffsetLS,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "handOrienationOffsetLS",
                                        ),
                                    );
                                }
                                m_handOrienationOffsetLS = _serde::__private::Some(
                                    match __A::next_value::<Quaternion>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(
                                    &m_maxElbowAngleDegrees,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "maxElbowAngleDegrees",
                                        ),
                                    );
                                }
                                m_maxElbowAngleDegrees = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(
                                    &m_minElbowAngleDegrees,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "minElbowAngleDegrees",
                                        ),
                                    );
                                }
                                m_minElbowAngleDegrees = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(&m_shoulderIndex) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "shoulderIndex",
                                        ),
                                    );
                                }
                                m_shoulderIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(
                                    &m_shoulderSiblingIndex,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "shoulderSiblingIndex",
                                        ),
                                    );
                                }
                                m_shoulderSiblingIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            8usize => {
                                if _serde::__private::Option::is_some(&m_elbowIndex) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowIndex",
                                        ),
                                    );
                                }
                                m_elbowIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            9usize => {
                                if _serde::__private::Option::is_some(
                                    &m_elbowSiblingIndex,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowSiblingIndex",
                                        ),
                                    );
                                }
                                m_elbowSiblingIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            10usize => {
                                if _serde::__private::Option::is_some(&m_wristIndex) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "wristIndex",
                                        ),
                                    );
                                }
                                m_wristIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            11usize => {
                                if _serde::__private::Option::is_some(
                                    &m_enforceEndPosition,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "enforceEndPosition",
                                        ),
                                    );
                                }
                                m_enforceEndPosition = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            12usize => {
                                if _serde::__private::Option::is_some(
                                    &m_enforceEndRotation,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "enforceEndRotation",
                                        ),
                                    );
                                }
                                m_enforceEndRotation = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            13usize => {
                                if _serde::__private::Option::is_some(&m_localFrameName) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "localFrameName",
                                        ),
                                    );
                                }
                                __A::pad(&mut __map, 0usize, 4usize)?;
                                m_localFrameName = _serde::__private::Some(
                                    match __A::next_value::<StringPtr<'de>>(&mut __map) {
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
                    __A::pad(&mut __map, 8usize, 0usize)?;
                    let m_elbowAxisLS = match m_elbowAxisLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowAxisLS",
                                ),
                            );
                        }
                    };
                    let m_backHandNormalLS = match m_backHandNormalLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "backHandNormalLS",
                                ),
                            );
                        }
                    };
                    let m_handOffsetLS = match m_handOffsetLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "handOffsetLS",
                                ),
                            );
                        }
                    };
                    let m_handOrienationOffsetLS = match m_handOrienationOffsetLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "handOrienationOffsetLS",
                                ),
                            );
                        }
                    };
                    let m_maxElbowAngleDegrees = match m_maxElbowAngleDegrees {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "maxElbowAngleDegrees",
                                ),
                            );
                        }
                    };
                    let m_minElbowAngleDegrees = match m_minElbowAngleDegrees {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "minElbowAngleDegrees",
                                ),
                            );
                        }
                    };
                    let m_shoulderIndex = match m_shoulderIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "shoulderIndex",
                                ),
                            );
                        }
                    };
                    let m_shoulderSiblingIndex = match m_shoulderSiblingIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "shoulderSiblingIndex",
                                ),
                            );
                        }
                    };
                    let m_elbowIndex = match m_elbowIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowIndex",
                                ),
                            );
                        }
                    };
                    let m_elbowSiblingIndex = match m_elbowSiblingIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowSiblingIndex",
                                ),
                            );
                        }
                    };
                    let m_wristIndex = match m_wristIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "wristIndex",
                                ),
                            );
                        }
                    };
                    let m_enforceEndPosition = match m_enforceEndPosition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "enforceEndPosition",
                                ),
                            );
                        }
                    };
                    let m_enforceEndRotation = match m_enforceEndRotation {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "enforceEndRotation",
                                ),
                            );
                        }
                    };
                    let m_localFrameName = match m_localFrameName {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "localFrameName",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbHandIkDriverInfoHand {
                        __ptr,
                        m_elbowAxisLS,
                        m_backHandNormalLS,
                        m_handOffsetLS,
                        m_handOrienationOffsetLS,
                        m_maxElbowAngleDegrees,
                        m_minElbowAngleDegrees,
                        m_shoulderIndex,
                        m_shoulderSiblingIndex,
                        m_elbowIndex,
                        m_elbowSiblingIndex,
                        m_wristIndex,
                        m_enforceEndPosition,
                        m_enforceEndRotation,
                        m_localFrameName,
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
                    let mut m_elbowAxisLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_backHandNormalLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_handOffsetLS: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_handOrienationOffsetLS: _serde::__private::Option<
                        Quaternion,
                    > = _serde::__private::None;
                    let mut m_maxElbowAngleDegrees: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_minElbowAngleDegrees: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_shoulderIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_shoulderSiblingIndex: _serde::__private::Option<
                        I16<'de>,
                    > = _serde::__private::None;
                    let mut m_elbowIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_elbowSiblingIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_wristIndex: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_enforceEndPosition: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_enforceEndRotation: _serde::__private::Option<bool> = _serde::__private::None;
                    let mut m_localFrameName: _serde::__private::Option<
                        StringPtr<'de>,
                    > = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_elbowAxisLS => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_elbowAxisLS) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowAxisLS",
                                        ),
                                    );
                                }
                                m_elbowAxisLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_backHandNormalLS => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_backHandNormalLS) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "backHandNormalLS",
                                        ),
                                    );
                                }
                                m_backHandNormalLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_handOffsetLS => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_handOffsetLS) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "handOffsetLS",
                                        ),
                                    );
                                }
                                m_handOffsetLS = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_handOrienationOffsetLS => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_handOrienationOffsetLS,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "handOrienationOffsetLS",
                                        ),
                                    );
                                }
                                m_handOrienationOffsetLS = _serde::__private::Some(
                                    match __A::next_value::<Quaternion>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_maxElbowAngleDegrees => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_maxElbowAngleDegrees,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "maxElbowAngleDegrees",
                                        ),
                                    );
                                }
                                m_maxElbowAngleDegrees = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_minElbowAngleDegrees => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_minElbowAngleDegrees,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "minElbowAngleDegrees",
                                        ),
                                    );
                                }
                                m_minElbowAngleDegrees = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_shoulderIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_shoulderIndex) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "shoulderIndex",
                                        ),
                                    );
                                }
                                m_shoulderIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_shoulderSiblingIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_shoulderSiblingIndex,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "shoulderSiblingIndex",
                                        ),
                                    );
                                }
                                m_shoulderSiblingIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_elbowIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_elbowIndex) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowIndex",
                                        ),
                                    );
                                }
                                m_elbowIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_elbowSiblingIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_elbowSiblingIndex,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "elbowSiblingIndex",
                                        ),
                                    );
                                }
                                m_elbowSiblingIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_wristIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_wristIndex) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "wristIndex",
                                        ),
                                    );
                                }
                                m_wristIndex = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_enforceEndPosition => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_enforceEndPosition,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "enforceEndPosition",
                                        ),
                                    );
                                }
                                m_enforceEndPosition = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_enforceEndRotation => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_enforceEndRotation,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "enforceEndRotation",
                                        ),
                                    );
                                }
                                m_enforceEndRotation = _serde::__private::Some(
                                    match __A::next_value::<bool>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_localFrameName => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_localFrameName) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "localFrameName",
                                        ),
                                    );
                                }
                                m_localFrameName = _serde::__private::Some(
                                    match __A::next_value::<StringPtr<'de>>(&mut __map) {
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
                    let m_elbowAxisLS = match m_elbowAxisLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowAxisLS",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_backHandNormalLS = match m_backHandNormalLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "backHandNormalLS",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_handOffsetLS = match m_handOffsetLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "handOffsetLS",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_handOrienationOffsetLS = match m_handOrienationOffsetLS {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "handOrienationOffsetLS",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_maxElbowAngleDegrees = match m_maxElbowAngleDegrees {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "maxElbowAngleDegrees",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_minElbowAngleDegrees = match m_minElbowAngleDegrees {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "minElbowAngleDegrees",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_shoulderIndex = match m_shoulderIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "shoulderIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_shoulderSiblingIndex = match m_shoulderSiblingIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "shoulderSiblingIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_elbowIndex = match m_elbowIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_elbowSiblingIndex = match m_elbowSiblingIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "elbowSiblingIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_wristIndex = match m_wristIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "wristIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_enforceEndPosition = match m_enforceEndPosition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "enforceEndPosition",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_enforceEndRotation = match m_enforceEndRotation {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "enforceEndRotation",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_localFrameName = match m_localFrameName {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "localFrameName",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkbHandIkDriverInfoHand {
                        __ptr: __ptr.clone(),
                        m_elbowAxisLS,
                        m_backHandNormalLS,
                        m_handOffsetLS,
                        m_handOrienationOffsetLS,
                        m_maxElbowAngleDegrees,
                        m_minElbowAngleDegrees,
                        m_shoulderIndex,
                        m_shoulderSiblingIndex,
                        m_elbowIndex,
                        m_elbowSiblingIndex,
                        m_wristIndex,
                        m_enforceEndPosition,
                        m_enforceEndRotation,
                        m_localFrameName,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "elbowAxisLS",
                "backHandNormalLS",
                "handOffsetLS",
                "handOrienationOffsetLS",
                "maxElbowAngleDegrees",
                "minElbowAngleDegrees",
                "shoulderIndex",
                "shoulderSiblingIndex",
                "elbowIndex",
                "elbowSiblingIndex",
                "wristIndex",
                "enforceEndPosition",
                "enforceEndRotation",
                "localFrameName",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbHandIkDriverInfoHand",
                FIELDS,
                __hkbHandIkDriverInfoHandVisitor {
                    marker: _serde::__private::PhantomData::<hkbHandIkDriverInfoHand>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
