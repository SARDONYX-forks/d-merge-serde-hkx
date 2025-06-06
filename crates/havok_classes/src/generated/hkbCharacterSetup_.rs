use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbCharacterSetup`
/// - version: `2`
/// - signature: `0xe5a2a413`
/// - size: ` 48`(x86)/` 88`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbCharacterSetup<'a> {
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
    /// - name: `retargetingSkeletonMappers`(ctype: `hkArray<hkaSkeletonMapper*>`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "retargetingSkeletonMappers"))]
    #[cfg_attr(feature = "serde", serde(rename = "retargetingSkeletonMappers"))]
    pub m_retargetingSkeletonMappers: Vec<Pointer<'a>>,
    /// # C++ Info
    /// - name: `animationSkeleton`(ctype: `struct hkaSkeleton*`)
    /// - offset: ` 20`(x86)/` 32`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "animationSkeleton"))]
    #[cfg_attr(feature = "serde", serde(rename = "animationSkeleton"))]
    pub m_animationSkeleton: Pointer<'a>,
    /// # C++ Info
    /// - name: `ragdollToAnimationSkeletonMapper`(ctype: `struct hkaSkeletonMapper*`)
    /// - offset: ` 24`(x86)/` 40`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(
        feature = "json_schema",
        schemars(rename = "ragdollToAnimationSkeletonMapper")
    )]
    #[cfg_attr(feature = "serde", serde(rename = "ragdollToAnimationSkeletonMapper"))]
    pub m_ragdollToAnimationSkeletonMapper: Pointer<'a>,
    /// # C++ Info
    /// - name: `animationToRagdollSkeletonMapper`(ctype: `struct hkaSkeletonMapper*`)
    /// - offset: ` 28`(x86)/` 48`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(
        feature = "json_schema",
        schemars(rename = "animationToRagdollSkeletonMapper")
    )]
    #[cfg_attr(feature = "serde", serde(rename = "animationToRagdollSkeletonMapper"))]
    pub m_animationToRagdollSkeletonMapper: Pointer<'a>,
    /// # C++ Info
    /// - name: `animationBindingSet`(ctype: `void*`)
    /// - offset: ` 32`(x86)/` 56`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "animationBindingSet"))]
    #[cfg_attr(feature = "serde", serde(rename = "animationBindingSet"))]
    pub m_animationBindingSet: Pointer<'a>,
    /// # C++ Info
    /// - name: `data`(ctype: `struct hkbCharacterData*`)
    /// - offset: ` 36`(x86)/` 64`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "data"))]
    #[cfg_attr(feature = "serde", serde(rename = "data"))]
    pub m_data: Pointer<'a>,
    /// # C++ Info
    /// - name: `mirroredSkeleton`(ctype: `void*`)
    /// - offset: ` 40`(x86)/` 72`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "mirroredSkeleton"))]
    #[cfg_attr(feature = "serde", serde(rename = "mirroredSkeleton"))]
    pub m_mirroredSkeleton: Pointer<'a>,
    /// # C++ Info
    /// - name: `characterPropertyIdMap`(ctype: `void*`)
    /// - offset: ` 44`(x86)/` 80`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "characterPropertyIdMap"))]
    #[cfg_attr(feature = "serde", serde(rename = "characterPropertyIdMap"))]
    pub m_characterPropertyIdMap: Pointer<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkbCharacterSetup<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbCharacterSetup"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xe5a2a413)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_retargetingSkeletonMappers.iter());
            v.push(&self.m_animationSkeleton);
            v.push(&self.m_ragdollToAnimationSkeletonMapper);
            v.push(&self.m_animationToRagdollSkeletonMapper);
            v.push(&self.m_animationBindingSet);
            v.push(&self.m_data);
            v.push(&self.m_mirroredSkeleton);
            v.push(&self.m_characterPropertyIdMap);
            v
        }
    }
    impl<'a> _serde::Serialize for hkbCharacterSetup<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xe5a2a413)));
            let mut serializer = __serializer
                .serialize_struct("hkbCharacterSetup", class_meta, (48u64, 88u64))?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer.skip_field("memSizeAndFlags", &self.parent.m_memSizeAndFlags)?;
            serializer.skip_field("referenceCount", &self.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer
                .serialize_array_field(
                    "retargetingSkeletonMappers",
                    &self.m_retargetingSkeletonMappers,
                    TypeSize::NonPtr,
                )?;
            serializer.serialize_field("animationSkeleton", &self.m_animationSkeleton)?;
            serializer
                .serialize_field(
                    "ragdollToAnimationSkeletonMapper",
                    &self.m_ragdollToAnimationSkeletonMapper,
                )?;
            serializer
                .serialize_field(
                    "animationToRagdollSkeletonMapper",
                    &self.m_animationToRagdollSkeletonMapper,
                )?;
            serializer.skip_field("animationBindingSet", &self.m_animationBindingSet)?;
            serializer.serialize_field("data", &self.m_data)?;
            serializer.skip_field("mirroredSkeleton", &self.m_mirroredSkeleton)?;
            serializer
                .skip_field("characterPropertyIdMap", &self.m_characterPropertyIdMap)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkbCharacterSetup<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_retargetingSkeletonMappers,
                m_animationSkeleton,
                m_ragdollToAnimationSkeletonMapper,
                m_animationToRagdollSkeletonMapper,
                m_data,
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
                        "retargetingSkeletonMappers" => {
                            Ok(__Field::m_retargetingSkeletonMappers)
                        }
                        "animationSkeleton" => Ok(__Field::m_animationSkeleton),
                        "ragdollToAnimationSkeletonMapper" => {
                            Ok(__Field::m_ragdollToAnimationSkeletonMapper)
                        }
                        "animationToRagdollSkeletonMapper" => {
                            Ok(__Field::m_animationToRagdollSkeletonMapper)
                        }
                        "data" => Ok(__Field::m_data),
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
            struct __hkbCharacterSetupVisitor<'de> {
                marker: _serde::__private::PhantomData<hkbCharacterSetup<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de> for __hkbCharacterSetupVisitor<'de> {
                type Value = hkbCharacterSetup<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbCharacterSetup",
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
                    let mut m_retargetingSkeletonMappers: _serde::__private::Option<
                        Vec<Pointer<'de>>,
                    > = _serde::__private::None;
                    let mut m_animationSkeleton: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_ragdollToAnimationSkeletonMapper: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_animationToRagdollSkeletonMapper: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_animationBindingSet: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_data: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_mirroredSkeleton: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_characterPropertyIdMap: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    for i in 0..8usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(
                                    &m_retargetingSkeletonMappers,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "retargetingSkeletonMappers",
                                        ),
                                    );
                                }
                                m_retargetingSkeletonMappers = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(
                                    &m_animationSkeleton,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "animationSkeleton",
                                        ),
                                    );
                                }
                                m_animationSkeleton = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(
                                    &m_ragdollToAnimationSkeletonMapper,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "ragdollToAnimationSkeletonMapper",
                                        ),
                                    );
                                }
                                m_ragdollToAnimationSkeletonMapper = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(
                                    &m_animationToRagdollSkeletonMapper,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "animationToRagdollSkeletonMapper",
                                        ),
                                    );
                                }
                                m_animationToRagdollSkeletonMapper = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(
                                    &m_animationBindingSet,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "animationBindingSet",
                                        ),
                                    );
                                }
                                m_animationBindingSet = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(&m_data) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("data"),
                                    );
                                }
                                m_data = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(&m_mirroredSkeleton) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "mirroredSkeleton",
                                        ),
                                    );
                                }
                                m_mirroredSkeleton = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(
                                    &m_characterPropertyIdMap,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "characterPropertyIdMap",
                                        ),
                                    );
                                }
                                m_characterPropertyIdMap = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
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
                    let m_retargetingSkeletonMappers = match m_retargetingSkeletonMappers {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "retargetingSkeletonMappers",
                                ),
                            );
                        }
                    };
                    let m_animationSkeleton = match m_animationSkeleton {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "animationSkeleton",
                                ),
                            );
                        }
                    };
                    let m_ragdollToAnimationSkeletonMapper = match m_ragdollToAnimationSkeletonMapper {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "ragdollToAnimationSkeletonMapper",
                                ),
                            );
                        }
                    };
                    let m_animationToRagdollSkeletonMapper = match m_animationToRagdollSkeletonMapper {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "animationToRagdollSkeletonMapper",
                                ),
                            );
                        }
                    };
                    let m_animationBindingSet = match m_animationBindingSet {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "animationBindingSet",
                                ),
                            );
                        }
                    };
                    let m_data = match m_data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("data"),
                            );
                        }
                    };
                    let m_mirroredSkeleton = match m_mirroredSkeleton {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "mirroredSkeleton",
                                ),
                            );
                        }
                    };
                    let m_characterPropertyIdMap = match m_characterPropertyIdMap {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "characterPropertyIdMap",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbCharacterSetup {
                        __ptr,
                        parent,
                        m_retargetingSkeletonMappers,
                        m_animationSkeleton,
                        m_ragdollToAnimationSkeletonMapper,
                        m_animationToRagdollSkeletonMapper,
                        m_animationBindingSet,
                        m_data,
                        m_mirroredSkeleton,
                        m_characterPropertyIdMap,
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
                    let mut m_retargetingSkeletonMappers: _serde::__private::Option<
                        Vec<Pointer<'de>>,
                    > = _serde::__private::None;
                    let mut m_animationSkeleton: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_ragdollToAnimationSkeletonMapper: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_animationToRagdollSkeletonMapper: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_data: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_retargetingSkeletonMappers => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_retargetingSkeletonMappers,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "retargetingSkeletonMappers",
                                        ),
                                    );
                                }
                                m_retargetingSkeletonMappers = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_animationSkeleton => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_animationSkeleton,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "animationSkeleton",
                                        ),
                                    );
                                }
                                m_animationSkeleton = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_ragdollToAnimationSkeletonMapper => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_ragdollToAnimationSkeletonMapper,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "ragdollToAnimationSkeletonMapper",
                                        ),
                                    );
                                }
                                m_ragdollToAnimationSkeletonMapper = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_animationToRagdollSkeletonMapper => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_animationToRagdollSkeletonMapper,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "animationToRagdollSkeletonMapper",
                                        ),
                                    );
                                }
                                m_animationToRagdollSkeletonMapper = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_data => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_data) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("data"),
                                    );
                                }
                                m_data = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
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
                    let m_retargetingSkeletonMappers = match m_retargetingSkeletonMappers {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "retargetingSkeletonMappers",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_animationSkeleton = match m_animationSkeleton {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "animationSkeleton",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_ragdollToAnimationSkeletonMapper = match m_ragdollToAnimationSkeletonMapper {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "ragdollToAnimationSkeletonMapper",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_animationToRagdollSkeletonMapper = match m_animationToRagdollSkeletonMapper {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "animationToRagdollSkeletonMapper",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_data = match m_data {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("data"),
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
                    _serde::__private::Ok(hkbCharacterSetup {
                        __ptr: __ptr.clone(),
                        parent,
                        m_retargetingSkeletonMappers,
                        m_animationSkeleton,
                        m_ragdollToAnimationSkeletonMapper,
                        m_animationToRagdollSkeletonMapper,
                        m_data,
                        ..Default::default()
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "retargetingSkeletonMappers",
                "animationSkeleton",
                "ragdollToAnimationSkeletonMapper",
                "animationToRagdollSkeletonMapper",
                "animationBindingSet",
                "data",
                "mirroredSkeleton",
                "characterPropertyIdMap",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbCharacterSetup",
                FIELDS,
                __hkbCharacterSetupVisitor {
                    marker: _serde::__private::PhantomData::<hkbCharacterSetup>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
