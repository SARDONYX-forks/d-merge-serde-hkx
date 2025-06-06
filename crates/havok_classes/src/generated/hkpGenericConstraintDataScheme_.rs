use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpGenericConstraintDataScheme`
/// - version: `0`
/// - signature: `0x11fd6f6c`
/// - size: ` 64`(x86)/` 80`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpGenericConstraintDataScheme<'a> {
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
    /// - name: `info`(ctype: `struct hkpGenericConstraintDataSchemeConstraintInfo`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: ` 16`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "info"))]
    #[cfg_attr(feature = "serde", serde(rename = "info"))]
    pub m_info: hkpGenericConstraintDataSchemeConstraintInfo<'a>,
    /// # C++ Info
    /// - name: `data`(ctype: `hkArray<hkVector4>`)
    /// - offset: ` 16`(x86)/` 16`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "data"))]
    #[cfg_attr(feature = "serde", serde(rename = "data"))]
    pub m_data: Vec<Vector4>,
    /// # C++ Info
    /// - name: `commands`(ctype: `hkArray<hkInt32>`)
    /// - offset: ` 28`(x86)/` 32`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "commands"))]
    #[cfg_attr(feature = "serde", serde(rename = "commands"))]
    pub m_commands: Vec<I32<'a>>,
    /// # C++ Info
    /// - name: `modifiers`(ctype: `hkArray<void*>`)
    /// - offset: ` 40`(x86)/` 48`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "modifiers"))]
    #[cfg_attr(feature = "serde", serde(rename = "modifiers"))]
    pub m_modifiers: Vec<Pointer<'a>>,
    /// # C++ Info
    /// - name: `motors`(ctype: `hkArray<hkpConstraintMotor*>`)
    /// - offset: ` 52`(x86)/` 64`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "motors"))]
    #[cfg_attr(feature = "serde", serde(rename = "motors"))]
    pub m_motors: Vec<Pointer<'a>>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpGenericConstraintDataScheme<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpGenericConstraintDataScheme"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0x11fd6f6c)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.extend(self.m_info.deps_indexes());
            v.extend(self.m_modifiers.iter());
            v.extend(self.m_motors.iter());
            v
        }
    }
    impl<'a> _serde::Serialize for hkpGenericConstraintDataScheme<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0x11fd6f6c)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkpGenericConstraintDataScheme",
                    class_meta,
                    (64u64, 80u64),
                )?;
            serializer.skip_field("info", &self.m_info)?;
            serializer.serialize_array_field("data", &self.m_data, TypeSize::NonPtr)?;
            serializer
                .serialize_array_field("commands", &self.m_commands, TypeSize::NonPtr)?;
            serializer
                .skip_array_field("modifiers", &self.m_modifiers, TypeSize::NonPtr)?;
            serializer
                .serialize_array_field("motors", &self.m_motors, TypeSize::NonPtr)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkpGenericConstraintDataScheme<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_data,
                m_commands,
                m_motors,
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
                        "data" => Ok(__Field::m_data),
                        "commands" => Ok(__Field::m_commands),
                        "motors" => Ok(__Field::m_motors),
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
            struct __hkpGenericConstraintDataSchemeVisitor<'de> {
                marker: _serde::__private::PhantomData<
                    hkpGenericConstraintDataScheme<'de>,
                >,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkpGenericConstraintDataSchemeVisitor<'de> {
                type Value = hkpGenericConstraintDataScheme<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkpGenericConstraintDataScheme",
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
                    let mut m_info: _serde::__private::Option<
                        hkpGenericConstraintDataSchemeConstraintInfo<'de>,
                    > = _serde::__private::None;
                    let mut m_data: _serde::__private::Option<Vec<Vector4>> = _serde::__private::None;
                    let mut m_commands: _serde::__private::Option<Vec<I32<'de>>> = _serde::__private::None;
                    let mut m_modifiers: _serde::__private::Option<Vec<Pointer<'de>>> = _serde::__private::None;
                    let mut m_motors: _serde::__private::Option<Vec<Pointer<'de>>> = _serde::__private::None;
                    for i in 0..5usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_info) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("info"),
                                    );
                                }
                                m_info = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpGenericConstraintDataSchemeConstraintInfo<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_data) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("data"),
                                    );
                                }
                                m_data = _serde::__private::Some(
                                    match __A::next_value::<Vec<Vector4>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_commands) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "commands",
                                        ),
                                    );
                                }
                                m_commands = _serde::__private::Some(
                                    match __A::next_value::<Vec<I32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_modifiers) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "modifiers",
                                        ),
                                    );
                                }
                                m_modifiers = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_motors) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("motors"),
                                    );
                                }
                                m_motors = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
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
                    let m_info = match m_info {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("info"),
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
                    let m_commands = match m_commands {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("commands"),
                            );
                        }
                    };
                    let m_modifiers = match m_modifiers {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "modifiers",
                                ),
                            );
                        }
                    };
                    let m_motors = match m_motors {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("motors"),
                            );
                        }
                    };
                    _serde::__private::Ok(hkpGenericConstraintDataScheme {
                        __ptr,
                        m_info,
                        m_data,
                        m_commands,
                        m_modifiers,
                        m_motors,
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
                    let mut m_data: _serde::__private::Option<Vec<Vector4>> = _serde::__private::None;
                    let mut m_commands: _serde::__private::Option<Vec<I32<'de>>> = _serde::__private::None;
                    let mut m_motors: _serde::__private::Option<Vec<Pointer<'de>>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
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
                                    match __A::next_value::<Vec<Vector4>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_commands => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_commands) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "commands",
                                        ),
                                    );
                                }
                                m_commands = _serde::__private::Some(
                                    match __A::next_value::<Vec<I32<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_motors => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_motors) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("motors"),
                                    );
                                }
                                m_motors = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
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
                    let m_commands = match m_commands {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("commands"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_motors = match m_motors {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("motors"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpGenericConstraintDataScheme {
                        __ptr: __ptr.clone(),
                        m_data,
                        m_commands,
                        m_motors,
                        ..Default::default()
                    })
                }
            }
            const FIELDS: &[&str] = &["info", "data", "commands", "modifiers", "motors"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpGenericConstraintDataScheme",
                FIELDS,
                __hkpGenericConstraintDataSchemeVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkpGenericConstraintDataScheme,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
