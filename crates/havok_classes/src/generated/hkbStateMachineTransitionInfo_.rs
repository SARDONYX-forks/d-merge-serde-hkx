use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbStateMachineTransitionInfo`
/// - version: `1`
/// - signature: `0xcdec8025`
/// - size: ` 60`(x86)/` 72`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbStateMachineTransitionInfo<'a> {
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
    /// - name: `triggerInterval`(ctype: `struct hkbStateMachineTimeInterval`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "triggerInterval"))]
    #[cfg_attr(feature = "serde", serde(rename = "triggerInterval"))]
    pub m_triggerInterval: hkbStateMachineTimeInterval<'a>,
    /// # C++ Info
    /// - name: `initiateInterval`(ctype: `struct hkbStateMachineTimeInterval`)
    /// - offset: ` 16`(x86)/` 16`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "initiateInterval"))]
    #[cfg_attr(feature = "serde", serde(rename = "initiateInterval"))]
    pub m_initiateInterval: hkbStateMachineTimeInterval<'a>,
    /// # C++ Info
    /// - name: `transition`(ctype: `struct hkbTransitionEffect*`)
    /// - offset: ` 32`(x86)/` 32`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "transition"))]
    #[cfg_attr(feature = "serde", serde(rename = "transition"))]
    pub m_transition: Pointer<'a>,
    /// # C++ Info
    /// - name: `condition`(ctype: `struct hkbCondition*`)
    /// - offset: ` 36`(x86)/` 40`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "condition"))]
    #[cfg_attr(feature = "serde", serde(rename = "condition"))]
    pub m_condition: Pointer<'a>,
    /// # C++ Info
    /// - name: `eventId`(ctype: `hkInt32`)
    /// - offset: ` 40`(x86)/` 48`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "eventId"))]
    #[cfg_attr(feature = "serde", serde(rename = "eventId"))]
    pub m_eventId: I32<'a>,
    /// # C++ Info
    /// - name: `toStateId`(ctype: `hkInt32`)
    /// - offset: ` 44`(x86)/` 52`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "toStateId"))]
    #[cfg_attr(feature = "serde", serde(rename = "toStateId"))]
    pub m_toStateId: I32<'a>,
    /// # C++ Info
    /// - name: `fromNestedStateId`(ctype: `hkInt32`)
    /// - offset: ` 48`(x86)/` 56`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "fromNestedStateId"))]
    #[cfg_attr(feature = "serde", serde(rename = "fromNestedStateId"))]
    pub m_fromNestedStateId: I32<'a>,
    /// # C++ Info
    /// - name: `toNestedStateId`(ctype: `hkInt32`)
    /// - offset: ` 52`(x86)/` 60`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "toNestedStateId"))]
    #[cfg_attr(feature = "serde", serde(rename = "toNestedStateId"))]
    pub m_toNestedStateId: I32<'a>,
    /// # C++ Info
    /// - name: `priority`(ctype: `hkInt16`)
    /// - offset: ` 56`(x86)/` 64`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "priority"))]
    #[cfg_attr(feature = "serde", serde(rename = "priority"))]
    pub m_priority: I16<'a>,
    /// # C++ Info
    /// - name: `flags`(ctype: `flags TransitionFlags`)
    /// - offset: ` 58`(x86)/` 66`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "flags"))]
    #[cfg_attr(feature = "serde", serde(rename = "flags"))]
    pub m_flags: TransitionFlags,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkbStateMachineTransitionInfo<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbStateMachineTransitionInfo"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xcdec8025)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_triggerInterval.deps_indexes());
            v.extend(self.m_initiateInterval.deps_indexes());
            v.push(&self.m_transition);
            v.push(&self.m_condition);
            v
        }
    }
    impl<'a> _serde::Serialize for hkbStateMachineTransitionInfo<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xcdec8025)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkbStateMachineTransitionInfo",
                    class_meta,
                    (60u64, 72u64),
                )?;
            serializer.serialize_field("triggerInterval", &self.m_triggerInterval)?;
            serializer.serialize_field("initiateInterval", &self.m_initiateInterval)?;
            serializer.serialize_field("transition", &self.m_transition)?;
            serializer.serialize_field("condition", &self.m_condition)?;
            serializer.serialize_field("eventId", &self.m_eventId)?;
            serializer.serialize_field("toStateId", &self.m_toStateId)?;
            serializer.serialize_field("fromNestedStateId", &self.m_fromNestedStateId)?;
            serializer.serialize_field("toNestedStateId", &self.m_toNestedStateId)?;
            serializer.serialize_field("priority", &self.m_priority)?;
            serializer.serialize_field("flags", &self.m_flags)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkbStateMachineTransitionInfo<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_triggerInterval,
                m_initiateInterval,
                m_transition,
                m_condition,
                m_eventId,
                m_toStateId,
                m_fromNestedStateId,
                m_toNestedStateId,
                m_priority,
                m_flags,
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
                        "triggerInterval" => Ok(__Field::m_triggerInterval),
                        "initiateInterval" => Ok(__Field::m_initiateInterval),
                        "transition" => Ok(__Field::m_transition),
                        "condition" => Ok(__Field::m_condition),
                        "eventId" => Ok(__Field::m_eventId),
                        "toStateId" => Ok(__Field::m_toStateId),
                        "fromNestedStateId" => Ok(__Field::m_fromNestedStateId),
                        "toNestedStateId" => Ok(__Field::m_toNestedStateId),
                        "priority" => Ok(__Field::m_priority),
                        "flags" => Ok(__Field::m_flags),
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
            struct __hkbStateMachineTransitionInfoVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkbStateMachineTransitionInfo<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkbStateMachineTransitionInfoVisitor<'de> {
                type Value = hkbStateMachineTransitionInfo<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbStateMachineTransitionInfo",
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
                    let mut m_triggerInterval: _serde::__private::Option<
                        hkbStateMachineTimeInterval,
                    > = _serde::__private::None;
                    let mut m_initiateInterval: _serde::__private::Option<
                        hkbStateMachineTimeInterval,
                    > = _serde::__private::None;
                    let mut m_transition: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_condition: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_eventId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_toStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_fromNestedStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_toNestedStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_priority: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_flags: _serde::__private::Option<TransitionFlags> = _serde::__private::None;
                    for i in 0..10usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_triggerInterval) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "triggerInterval",
                                        ),
                                    );
                                }
                                m_triggerInterval = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkbStateMachineTimeInterval,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_initiateInterval) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "initiateInterval",
                                        ),
                                    );
                                }
                                m_initiateInterval = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkbStateMachineTimeInterval,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_transition) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "transition",
                                        ),
                                    );
                                }
                                m_transition = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_condition) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "condition",
                                        ),
                                    );
                                }
                                m_condition = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_eventId) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "eventId",
                                        ),
                                    );
                                }
                                m_eventId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(&m_toStateId) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "toStateId",
                                        ),
                                    );
                                }
                                m_toStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(
                                    &m_fromNestedStateId,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "fromNestedStateId",
                                        ),
                                    );
                                }
                                m_fromNestedStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(&m_toNestedStateId) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "toNestedStateId",
                                        ),
                                    );
                                }
                                m_toNestedStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            8usize => {
                                if _serde::__private::Option::is_some(&m_priority) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "priority",
                                        ),
                                    );
                                }
                                m_priority = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            9usize => {
                                if _serde::__private::Option::is_some(&m_flags) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("flags"),
                                    );
                                }
                                m_flags = _serde::__private::Some(
                                    match __A::next_value::<TransitionFlags>(&mut __map) {
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
                    __A::pad(&mut __map, 0usize, 4usize)?;
                    let m_triggerInterval = match m_triggerInterval {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "triggerInterval",
                                ),
                            );
                        }
                    };
                    let m_initiateInterval = match m_initiateInterval {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "initiateInterval",
                                ),
                            );
                        }
                    };
                    let m_transition = match m_transition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "transition",
                                ),
                            );
                        }
                    };
                    let m_condition = match m_condition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "condition",
                                ),
                            );
                        }
                    };
                    let m_eventId = match m_eventId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("eventId"),
                            );
                        }
                    };
                    let m_toStateId = match m_toStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "toStateId",
                                ),
                            );
                        }
                    };
                    let m_fromNestedStateId = match m_fromNestedStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "fromNestedStateId",
                                ),
                            );
                        }
                    };
                    let m_toNestedStateId = match m_toNestedStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "toNestedStateId",
                                ),
                            );
                        }
                    };
                    let m_priority = match m_priority {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("priority"),
                            );
                        }
                    };
                    let m_flags = match m_flags {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("flags"),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbStateMachineTransitionInfo {
                        __ptr,
                        m_triggerInterval,
                        m_initiateInterval,
                        m_transition,
                        m_condition,
                        m_eventId,
                        m_toStateId,
                        m_fromNestedStateId,
                        m_toNestedStateId,
                        m_priority,
                        m_flags,
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
                    let mut m_triggerInterval: _serde::__private::Option<
                        hkbStateMachineTimeInterval,
                    > = _serde::__private::None;
                    let mut m_initiateInterval: _serde::__private::Option<
                        hkbStateMachineTimeInterval,
                    > = _serde::__private::None;
                    let mut m_transition: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_condition: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_eventId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_toStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_fromNestedStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_toNestedStateId: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    let mut m_priority: _serde::__private::Option<I16<'de>> = _serde::__private::None;
                    let mut m_flags: _serde::__private::Option<TransitionFlags> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_triggerInterval => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_triggerInterval) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "triggerInterval",
                                        ),
                                    );
                                }
                                m_triggerInterval = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkbStateMachineTimeInterval,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_initiateInterval => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_initiateInterval) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "initiateInterval",
                                        ),
                                    );
                                }
                                m_initiateInterval = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkbStateMachineTimeInterval,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_transition => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_transition) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "transition",
                                        ),
                                    );
                                }
                                m_transition = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_condition => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_condition) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "condition",
                                        ),
                                    );
                                }
                                m_condition = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_eventId => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_eventId) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "eventId",
                                        ),
                                    );
                                }
                                m_eventId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_toStateId => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_toStateId) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "toStateId",
                                        ),
                                    );
                                }
                                m_toStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_fromNestedStateId => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_fromNestedStateId,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "fromNestedStateId",
                                        ),
                                    );
                                }
                                m_fromNestedStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_toNestedStateId => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_toNestedStateId) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "toNestedStateId",
                                        ),
                                    );
                                }
                                m_toNestedStateId = _serde::__private::Some(
                                    match __A::next_value::<I32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_priority => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_priority) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "priority",
                                        ),
                                    );
                                }
                                m_priority = _serde::__private::Some(
                                    match __A::next_value::<I16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_flags => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_flags) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("flags"),
                                    );
                                }
                                m_flags = _serde::__private::Some(
                                    match __A::next_value::<TransitionFlags>(&mut __map) {
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
                    let m_triggerInterval = match m_triggerInterval {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "triggerInterval",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_initiateInterval = match m_initiateInterval {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "initiateInterval",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_transition = match m_transition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "transition",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_condition = match m_condition {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "condition",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_eventId = match m_eventId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("eventId"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_toStateId = match m_toStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "toStateId",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_fromNestedStateId = match m_fromNestedStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "fromNestedStateId",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_toNestedStateId = match m_toNestedStateId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "toNestedStateId",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_priority = match m_priority {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("priority"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_flags = match m_flags {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("flags"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkbStateMachineTransitionInfo {
                        __ptr: __ptr.clone(),
                        m_triggerInterval,
                        m_initiateInterval,
                        m_transition,
                        m_condition,
                        m_eventId,
                        m_toStateId,
                        m_fromNestedStateId,
                        m_toNestedStateId,
                        m_priority,
                        m_flags,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "triggerInterval",
                "initiateInterval",
                "transition",
                "condition",
                "eventId",
                "toStateId",
                "fromNestedStateId",
                "toNestedStateId",
                "priority",
                "flags",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbStateMachineTransitionInfo",
                FIELDS,
                __hkbStateMachineTransitionInfoVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkbStateMachineTransitionInfo,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
#[havok_types_derive::impl_flags_methods]
bitflags::bitflags! {
    #[doc = r" Bit flags"] #[doc = r""] #[doc = r" # C++ Info"] #[doc =
    " - name: `TransitionFlags`(ctype: `hkFlags<TransitionFlags, hkInt16>`)"]
    #[allow(non_upper_case_globals, non_snake_case)] #[cfg_attr(feature = "serde",
    derive(serde_with::SerializeDisplay, serde_with::DeserializeFromStr))]
    #[repr(transparent)] #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct
    TransitionFlags : i16 { #[doc = "1"] const FLAG_USE_TRIGGER_INTERVAL = 1i16; #[doc =
    "2"] const FLAG_USE_INITIATE_INTERVAL = 2i16; #[doc = "4"] const
    FLAG_UNINTERRUPTIBLE_WHILE_PLAYING = 4i16; #[doc = "8"] const
    FLAG_UNINTERRUPTIBLE_WHILE_DELAYED = 8i16; #[doc = "16"] const
    FLAG_DELAY_STATE_CHANGE = 16i16; #[doc = "32"] const FLAG_DISABLED = 32i16; #[doc =
    "64"] const FLAG_DISALLOW_RETURN_TO_PREVIOUS_STATE = 64i16; #[doc = "128"] const
    FLAG_DISALLOW_RANDOM_TRANSITION = 128i16; #[doc = "256"] const FLAG_DISABLE_CONDITION
    = 256i16; #[doc = "512"] const
    FLAG_ALLOW_SELF_TRANSITION_BY_TRANSITION_FROM_ANY_STATE = 512i16; #[doc = "1024"]
    const FLAG_IS_GLOBAL_WILDCARD = 1024i16; #[doc = "2048"] const FLAG_IS_LOCAL_WILDCARD
    = 2048i16; #[doc = "4096"] const FLAG_FROM_NESTED_STATE_ID_IS_VALID = 4096i16; #[doc
    = "8192"] const FLAG_TO_NESTED_STATE_ID_IS_VALID = 8192i16; #[doc = "16384"] const
    FLAG_ABUT_AT_END_OF_FROM_GENERATOR = 16384i16; }
}
#[cfg(feature = "json_schema")]
const _: () = {
    use schemars::{SchemaGenerator, Schema, JsonSchema, json_schema};
    use std::borrow::Cow;
    impl JsonSchema for TransitionFlags {
        fn schema_name() -> Cow<'static, str> {
            "TransitionFlags".into()
        }
        fn schema_id() -> Cow<'static, str> {
            concat!(module_path!(), "::", "TransitionFlags").into()
        }
        fn json_schema(_generate: &mut SchemaGenerator) -> Schema {
            json_schema!(
                { "description" :
                "Bitflags field. Specific flags: FLAG_USE_TRIGGER_INTERVAL: 1, FLAG_USE_INITIATE_INTERVAL: 2, FLAG_UNINTERRUPTIBLE_WHILE_PLAYING: 4, FLAG_UNINTERRUPTIBLE_WHILE_DELAYED: 8, FLAG_DELAY_STATE_CHANGE: 16, FLAG_DISABLED: 32, FLAG_DISALLOW_RETURN_TO_PREVIOUS_STATE: 64, FLAG_DISALLOW_RANDOM_TRANSITION: 128, FLAG_DISABLE_CONDITION: 256, FLAG_ALLOW_SELF_TRANSITION_BY_TRANSITION_FROM_ANY_STATE: 512, FLAG_IS_GLOBAL_WILDCARD: 1024, FLAG_IS_LOCAL_WILDCARD: 2048, FLAG_FROM_NESTED_STATE_ID_IS_VALID: 4096, FLAG_TO_NESTED_STATE_ID_IS_VALID: 8192, FLAG_ABUT_AT_END_OF_FROM_GENERATOR: 16384. Additional unspecified bits may be set.(e.g.: BIT_FLAG|BIT_FLAG2|4)",
                "type" : "string", }
            )
        }
    }
};
const _: () = {
    use havok_serde as __serde;
    impl __serde::Serialize for TransitionFlags {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: __serde::ser::Serializer,
        {
            let mut __serializer = __serializer.serialize_enum_flags()?;
            if self.is_empty() {
                __serializer.serialize_bits(&self.bits())?;
                __serializer.serialize_empty_bit()?;
                return __serializer.end();
            }
            for flag in self.iter() {
                match flag {
                    Self::FLAG_USE_TRIGGER_INTERVAL => {
                        __serializer
                            .serialize_field(
                                "FLAG_USE_TRIGGER_INTERVAL",
                                &Self::FLAG_USE_TRIGGER_INTERVAL,
                            )
                    }
                    Self::FLAG_USE_INITIATE_INTERVAL => {
                        __serializer
                            .serialize_field(
                                "FLAG_USE_INITIATE_INTERVAL",
                                &Self::FLAG_USE_INITIATE_INTERVAL,
                            )
                    }
                    Self::FLAG_UNINTERRUPTIBLE_WHILE_PLAYING => {
                        __serializer
                            .serialize_field(
                                "FLAG_UNINTERRUPTIBLE_WHILE_PLAYING",
                                &Self::FLAG_UNINTERRUPTIBLE_WHILE_PLAYING,
                            )
                    }
                    Self::FLAG_UNINTERRUPTIBLE_WHILE_DELAYED => {
                        __serializer
                            .serialize_field(
                                "FLAG_UNINTERRUPTIBLE_WHILE_DELAYED",
                                &Self::FLAG_UNINTERRUPTIBLE_WHILE_DELAYED,
                            )
                    }
                    Self::FLAG_DELAY_STATE_CHANGE => {
                        __serializer
                            .serialize_field(
                                "FLAG_DELAY_STATE_CHANGE",
                                &Self::FLAG_DELAY_STATE_CHANGE,
                            )
                    }
                    Self::FLAG_DISABLED => {
                        __serializer
                            .serialize_field("FLAG_DISABLED", &Self::FLAG_DISABLED)
                    }
                    Self::FLAG_DISALLOW_RETURN_TO_PREVIOUS_STATE => {
                        __serializer
                            .serialize_field(
                                "FLAG_DISALLOW_RETURN_TO_PREVIOUS_STATE",
                                &Self::FLAG_DISALLOW_RETURN_TO_PREVIOUS_STATE,
                            )
                    }
                    Self::FLAG_DISALLOW_RANDOM_TRANSITION => {
                        __serializer
                            .serialize_field(
                                "FLAG_DISALLOW_RANDOM_TRANSITION",
                                &Self::FLAG_DISALLOW_RANDOM_TRANSITION,
                            )
                    }
                    Self::FLAG_DISABLE_CONDITION => {
                        __serializer
                            .serialize_field(
                                "FLAG_DISABLE_CONDITION",
                                &Self::FLAG_DISABLE_CONDITION,
                            )
                    }
                    Self::FLAG_ALLOW_SELF_TRANSITION_BY_TRANSITION_FROM_ANY_STATE => {
                        __serializer
                            .serialize_field(
                                "FLAG_ALLOW_SELF_TRANSITION_BY_TRANSITION_FROM_ANY_STATE",
                                &Self::FLAG_ALLOW_SELF_TRANSITION_BY_TRANSITION_FROM_ANY_STATE,
                            )
                    }
                    Self::FLAG_IS_GLOBAL_WILDCARD => {
                        __serializer
                            .serialize_field(
                                "FLAG_IS_GLOBAL_WILDCARD",
                                &Self::FLAG_IS_GLOBAL_WILDCARD,
                            )
                    }
                    Self::FLAG_IS_LOCAL_WILDCARD => {
                        __serializer
                            .serialize_field(
                                "FLAG_IS_LOCAL_WILDCARD",
                                &Self::FLAG_IS_LOCAL_WILDCARD,
                            )
                    }
                    Self::FLAG_FROM_NESTED_STATE_ID_IS_VALID => {
                        __serializer
                            .serialize_field(
                                "FLAG_FROM_NESTED_STATE_ID_IS_VALID",
                                &Self::FLAG_FROM_NESTED_STATE_ID_IS_VALID,
                            )
                    }
                    Self::FLAG_TO_NESTED_STATE_ID_IS_VALID => {
                        __serializer
                            .serialize_field(
                                "FLAG_TO_NESTED_STATE_ID_IS_VALID",
                                &Self::FLAG_TO_NESTED_STATE_ID_IS_VALID,
                            )
                    }
                    Self::FLAG_ABUT_AT_END_OF_FROM_GENERATOR => {
                        __serializer
                            .serialize_field(
                                "FLAG_ABUT_AT_END_OF_FROM_GENERATOR",
                                &Self::FLAG_ABUT_AT_END_OF_FROM_GENERATOR,
                            )
                    }
                    remain => {
                        __serializer
                            .serialize_field(&remain.bits().to_string(), &remain.bits())
                    }
                }?;
            }
            __serializer.serialize_bits(&self.bits())?;
            __serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    #[allow(unused_extern_crates, clippy::useless_attribute)]
    extern crate havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for TransitionFlags {
        fn deserialize<__D>(
            __deserializer: __D,
        ) -> _serde::__private::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[doc(hidden)]
            struct __Visitor<'de> {
                marker: _serde::__private::PhantomData<TransitionFlags>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TransitionFlags;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::__private::Formatter,
                ) -> _serde::__private::fmt::Result {
                    _serde::__private::Formatter::write_str(
                        __formatter,
                        "struct TransitionFlags(flags)",
                    )
                }
                #[inline]
                fn visit_int16<__E>(
                    self,
                    __value: I16<'de>,
                ) -> _serde::__private::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        I16::Number(__value) => {
                            Ok(TransitionFlags::from_bits_retain(__value as _))
                        }
                        _ => {
                            Err(
                                _serde::de::Error::invalid_value(
                                    _serde::de::Unexpected::Int16(__value as _),
                                    &"TransitionFlags(I16) Number",
                                ),
                            )
                        }
                    }
                }
                fn visit_stringptr<__E>(
                    self,
                    __value: StringPtr<'de>,
                ) -> _serde::__private::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match <TransitionFlags as core::str::FromStr>::from_str(
                        __value.into_inner().unwrap().as_ref(),
                    ) {
                        Ok(flags) => Ok(flags),
                        Err(err) => Err(_serde::de::Error::custom(err)),
                    }
                }
            }
            _serde::Deserializer::deserialize_flags(
                __deserializer,
                _serde::de::ReadEnumSize::Int16,
                __Visitor {
                    marker: _serde::__private::PhantomData::<TransitionFlags>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
