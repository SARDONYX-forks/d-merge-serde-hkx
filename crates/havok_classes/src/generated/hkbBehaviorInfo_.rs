use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbBehaviorInfo`
/// - version: `1`
/// - signature: `0xf7645395`
/// - size: ` 32`(x86)/` 48`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbBehaviorInfo<'a> {
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
    /// - name: `characterId`(ctype: `hkUint64`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: `  8`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "characterId"))]
    #[cfg_attr(feature = "serde", serde(rename = "characterId"))]
    pub m_characterId: U64<'a>,
    /// # C++ Info
    /// - name: `data`(ctype: `struct hkbBehaviorGraphData*`)
    /// - offset: ` 16`(x86)/` 24`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "data"))]
    #[cfg_attr(feature = "serde", serde(rename = "data"))]
    pub m_data: Pointer<'a>,
    /// # C++ Info
    /// - name: `idToNamePairs`(ctype: `hkArray<struct hkbBehaviorInfoIdToNamePair>`)
    /// - offset: ` 20`(x86)/` 32`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "idToNamePairs"))]
    #[cfg_attr(feature = "serde", serde(rename = "idToNamePairs"))]
    pub m_idToNamePairs: Vec<hkbBehaviorInfoIdToNamePair<'a>>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkbBehaviorInfo<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbBehaviorInfo"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xf7645395)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.push(&self.m_data);
            v.extend(
                self
                    .m_idToNamePairs
                    .iter()
                    .flat_map(|class| class.deps_indexes())
                    .collect::<Vec<&Pointer<'_>>>(),
            );
            v
        }
    }
    impl<'a> _serde::Serialize for hkbBehaviorInfo<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xf7645395)));
            let mut serializer = __serializer
                .serialize_struct("hkbBehaviorInfo", class_meta, (32u64, 48u64))?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer.skip_field("memSizeAndFlags", &self.parent.m_memSizeAndFlags)?;
            serializer.skip_field("referenceCount", &self.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.serialize_field("characterId", &self.m_characterId)?;
            serializer.serialize_field("data", &self.m_data)?;
            serializer
                .serialize_array_field(
                    "idToNamePairs",
                    &self.m_idToNamePairs,
                    TypeSize::Struct {
                        size_x86: 12u64,
                        size_x86_64: 24u64,
                    },
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
    impl<'de> _serde::Deserialize<'de> for hkbBehaviorInfo<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_characterId,
                m_data,
                m_idToNamePairs,
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
                        "characterId" => Ok(__Field::m_characterId),
                        "data" => Ok(__Field::m_data),
                        "idToNamePairs" => Ok(__Field::m_idToNamePairs),
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
            struct __hkbBehaviorInfoVisitor<'de> {
                marker: _serde::__private::PhantomData<hkbBehaviorInfo<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de> for __hkbBehaviorInfoVisitor<'de> {
                type Value = hkbBehaviorInfo<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbBehaviorInfo",
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
                    let mut m_characterId: _serde::__private::Option<U64<'de>> = _serde::__private::None;
                    let mut m_data: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_idToNamePairs: _serde::__private::Option<
                        Vec<hkbBehaviorInfoIdToNamePair<'de>>,
                    > = _serde::__private::None;
                    for i in 0..3usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_characterId) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "characterId",
                                        ),
                                    );
                                }
                                m_characterId = _serde::__private::Some(
                                    match __A::next_value::<U64<'de>>(&mut __map) {
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
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_idToNamePairs) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "idToNamePairs",
                                        ),
                                    );
                                }
                                m_idToNamePairs = _serde::__private::Some(
                                    match __A::next_value::<
                                        Vec<hkbBehaviorInfoIdToNamePair<'de>>,
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
                    let m_characterId = match m_characterId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "characterId",
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
                    let m_idToNamePairs = match m_idToNamePairs {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "idToNamePairs",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbBehaviorInfo {
                        __ptr,
                        parent,
                        m_characterId,
                        m_data,
                        m_idToNamePairs,
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
                    let mut m_characterId: _serde::__private::Option<U64<'de>> = _serde::__private::None;
                    let mut m_data: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_idToNamePairs: _serde::__private::Option<
                        Vec<hkbBehaviorInfoIdToNamePair<'de>>,
                    > = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_characterId => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_characterId) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "characterId",
                                        ),
                                    );
                                }
                                m_characterId = _serde::__private::Some(
                                    match __A::next_value::<U64<'de>>(&mut __map) {
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
                            __Field::m_idToNamePairs => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_idToNamePairs) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "idToNamePairs",
                                        ),
                                    );
                                }
                                m_idToNamePairs = _serde::__private::Some(
                                    match __A::next_value::<
                                        Vec<hkbBehaviorInfoIdToNamePair<'de>>,
                                    >(&mut __map) {
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
                    let m_characterId = match m_characterId {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "characterId",
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
                    let m_idToNamePairs = match m_idToNamePairs {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "idToNamePairs",
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
                    _serde::__private::Ok(hkbBehaviorInfo {
                        __ptr: __ptr.clone(),
                        parent,
                        m_characterId,
                        m_data,
                        m_idToNamePairs,
                    })
                }
            }
            const FIELDS: &[&str] = &["characterId", "data", "idToNamePairs"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbBehaviorInfo",
                FIELDS,
                __hkbBehaviorInfoVisitor {
                    marker: _serde::__private::PhantomData::<hkbBehaviorInfo>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
