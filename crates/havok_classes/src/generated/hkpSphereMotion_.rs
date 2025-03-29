use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpSphereMotion`
/// - version: `0`
/// - signature: `0xbafa2bb7`
/// - size: `288`(x86)/`320`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpSphereMotion<'a> {
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
    pub parent: hkpMotion<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpSphereMotion<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpSphereMotion"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xbafa2bb7)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.parent.m_motionState.deps_indexes());
            v.push(&self.parent.m_savedMotion);
            v
        }
    }
    impl<'a> _serde::Serialize for hkpSphereMotion<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xbafa2bb7)));
            let mut serializer = __serializer
                .serialize_struct("hkpSphereMotion", class_meta, (288u64, 320u64))?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer
                .skip_field("memSizeAndFlags", &self.parent.parent.m_memSizeAndFlags)?;
            serializer
                .skip_field("referenceCount", &self.parent.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.serialize_field("type", &self.parent.m_type)?;
            serializer
                .serialize_field(
                    "deactivationIntegrateCounter",
                    &self.parent.m_deactivationIntegrateCounter,
                )?;
            serializer
                .serialize_fixed_array_field(
                    "deactivationNumInactiveFrames",
                    self.parent.m_deactivationNumInactiveFrames.as_slice(),
                    TypeSize::NonPtr,
                )?;
            serializer.pad_field([0u8; 2usize].as_slice(), [0u8; 10usize].as_slice())?;
            serializer.serialize_field("motionState", &self.parent.m_motionState)?;
            serializer
                .serialize_field("inertiaAndMassInv", &self.parent.m_inertiaAndMassInv)?;
            serializer.serialize_field("linearVelocity", &self.parent.m_linearVelocity)?;
            serializer
                .serialize_field("angularVelocity", &self.parent.m_angularVelocity)?;
            serializer
                .serialize_fixed_array_field(
                    "deactivationRefPosition",
                    self.parent.m_deactivationRefPosition.as_slice(),
                    TypeSize::NonPtr,
                )?;
            serializer
                .serialize_fixed_array_field(
                    "deactivationRefOrientation",
                    self.parent.m_deactivationRefOrientation.as_slice(),
                    TypeSize::NonPtr,
                )?;
            serializer.serialize_field("savedMotion", &self.parent.m_savedMotion)?;
            serializer
                .serialize_field(
                    "savedQualityTypeIndex",
                    &self.parent.m_savedQualityTypeIndex,
                )?;
            serializer.serialize_field("gravityFactor", &self.parent.m_gravityFactor)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 12usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkpSphereMotion<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_type,
                m_deactivationIntegrateCounter,
                m_deactivationNumInactiveFrames,
                m_motionState,
                m_inertiaAndMassInv,
                m_linearVelocity,
                m_angularVelocity,
                m_deactivationRefPosition,
                m_deactivationRefOrientation,
                m_savedMotion,
                m_savedQualityTypeIndex,
                m_gravityFactor,
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
                        "type" => Ok(__Field::m_type),
                        "deactivationIntegrateCounter" => {
                            Ok(__Field::m_deactivationIntegrateCounter)
                        }
                        "deactivationNumInactiveFrames" => {
                            Ok(__Field::m_deactivationNumInactiveFrames)
                        }
                        "motionState" => Ok(__Field::m_motionState),
                        "inertiaAndMassInv" => Ok(__Field::m_inertiaAndMassInv),
                        "linearVelocity" => Ok(__Field::m_linearVelocity),
                        "angularVelocity" => Ok(__Field::m_angularVelocity),
                        "deactivationRefPosition" => {
                            Ok(__Field::m_deactivationRefPosition)
                        }
                        "deactivationRefOrientation" => {
                            Ok(__Field::m_deactivationRefOrientation)
                        }
                        "savedMotion" => Ok(__Field::m_savedMotion),
                        "savedQualityTypeIndex" => Ok(__Field::m_savedQualityTypeIndex),
                        "gravityFactor" => Ok(__Field::m_gravityFactor),
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
            struct __hkpSphereMotionVisitor<'de> {
                marker: _serde::__private::PhantomData<hkpSphereMotion<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de> for __hkpSphereMotionVisitor<'de> {
                type Value = hkpSphereMotion<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkpSphereMotion",
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
                    for i in 0..0usize {
                        match i {
                            _ => {}
                        }
                    }
                    _serde::__private::Ok(hkpSphereMotion { __ptr, parent })
                }
                #[allow(clippy::manual_unwrap_or_default)]
                fn visit_struct<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::__private::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut m_type: _serde::__private::Option<MotionType> = _serde::__private::None;
                    let mut m_deactivationIntegrateCounter: _serde::__private::Option<
                        U8<'de>,
                    > = _serde::__private::None;
                    let mut m_deactivationNumInactiveFrames: _serde::__private::Option<
                        [U16<'de>; 2usize],
                    > = _serde::__private::None;
                    let mut m_motionState: _serde::__private::Option<
                        hkMotionState<'de>,
                    > = _serde::__private::None;
                    let mut m_inertiaAndMassInv: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_linearVelocity: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_angularVelocity: _serde::__private::Option<Vector4> = _serde::__private::None;
                    let mut m_deactivationRefPosition: _serde::__private::Option<
                        [Vector4; 2usize],
                    > = _serde::__private::None;
                    let mut m_deactivationRefOrientation: _serde::__private::Option<
                        [U32<'de>; 2usize],
                    > = _serde::__private::None;
                    let mut m_savedMotion: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_savedQualityTypeIndex: _serde::__private::Option<
                        U16<'de>,
                    > = _serde::__private::None;
                    let mut m_gravityFactor: _serde::__private::Option<f16> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_type => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_type) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("type"),
                                    );
                                }
                                m_type = _serde::__private::Some(
                                    match __A::next_value::<MotionType>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_deactivationIntegrateCounter => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_deactivationIntegrateCounter,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "deactivationIntegrateCounter",
                                        ),
                                    );
                                }
                                m_deactivationIntegrateCounter = _serde::__private::Some(
                                    match __A::next_value::<U8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_deactivationNumInactiveFrames => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_deactivationNumInactiveFrames,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "deactivationNumInactiveFrames",
                                        ),
                                    );
                                }
                                m_deactivationNumInactiveFrames = _serde::__private::Some(
                                    match __A::next_value::<[U16<'de>; 2usize]>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_motionState => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_motionState) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "motionState",
                                        ),
                                    );
                                }
                                m_motionState = _serde::__private::Some(
                                    match __A::next_value::<hkMotionState<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_inertiaAndMassInv => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_inertiaAndMassInv,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "inertiaAndMassInv",
                                        ),
                                    );
                                }
                                m_inertiaAndMassInv = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_linearVelocity => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_linearVelocity) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "linearVelocity",
                                        ),
                                    );
                                }
                                m_linearVelocity = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_angularVelocity => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_angularVelocity) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "angularVelocity",
                                        ),
                                    );
                                }
                                m_angularVelocity = _serde::__private::Some(
                                    match __A::next_value::<Vector4>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_deactivationRefPosition => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_deactivationRefPosition,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "deactivationRefPosition",
                                        ),
                                    );
                                }
                                m_deactivationRefPosition = _serde::__private::Some(
                                    match __A::next_value::<[Vector4; 2usize]>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_deactivationRefOrientation => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_deactivationRefOrientation,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "deactivationRefOrientation",
                                        ),
                                    );
                                }
                                m_deactivationRefOrientation = _serde::__private::Some(
                                    match __A::next_value::<[U32<'de>; 2usize]>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_savedMotion => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_savedMotion) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "savedMotion",
                                        ),
                                    );
                                }
                                m_savedMotion = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_savedQualityTypeIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_savedQualityTypeIndex,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "savedQualityTypeIndex",
                                        ),
                                    );
                                }
                                m_savedQualityTypeIndex = _serde::__private::Some(
                                    match __A::next_value::<U16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_gravityFactor => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_gravityFactor) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "gravityFactor",
                                        ),
                                    );
                                }
                                m_gravityFactor = _serde::__private::Some(
                                    match __A::next_value::<f16>(&mut __map) {
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
                    let m_type = match m_type {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("type"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_deactivationIntegrateCounter = match m_deactivationIntegrateCounter {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "deactivationIntegrateCounter",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_deactivationNumInactiveFrames = match m_deactivationNumInactiveFrames {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "deactivationNumInactiveFrames",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_motionState = match m_motionState {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "motionState",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_inertiaAndMassInv = match m_inertiaAndMassInv {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "inertiaAndMassInv",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_linearVelocity = match m_linearVelocity {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "linearVelocity",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_angularVelocity = match m_angularVelocity {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "angularVelocity",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_deactivationRefPosition = match m_deactivationRefPosition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "deactivationRefPosition",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_deactivationRefOrientation = match m_deactivationRefOrientation {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "deactivationRefOrientation",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_savedMotion = match m_savedMotion {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "savedMotion",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_savedQualityTypeIndex = match m_savedQualityTypeIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "savedQualityTypeIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_gravityFactor = match m_gravityFactor {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "gravityFactor",
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
                    let parent = hkpMotion {
                        __ptr: __ptr.clone(),
                        parent,
                        m_type,
                        m_deactivationIntegrateCounter,
                        m_deactivationNumInactiveFrames,
                        m_motionState,
                        m_inertiaAndMassInv,
                        m_linearVelocity,
                        m_angularVelocity,
                        m_deactivationRefPosition,
                        m_deactivationRefOrientation,
                        m_savedMotion,
                        m_savedQualityTypeIndex,
                        m_gravityFactor,
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpSphereMotion {
                        __ptr: __ptr.clone(),
                        parent,
                    })
                }
            }
            const FIELDS: &[&str] = &[];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpSphereMotion",
                FIELDS,
                __hkpSphereMotionVisitor {
                    marker: _serde::__private::PhantomData::<hkpSphereMotion>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
