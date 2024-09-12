use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkbIntVariableSequencedData`
/// - version: `0`
/// - signature: `0x7bfc518a`
/// - size: ` 24`(x86)/` 40`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkbIntVariableSequencedData {
    /// # Unique index for this class
    /// - Represents a pointer on XML (`<hkobject name="#0001"></hkobject>`)
    /// - [`Option::None`] => This class is `class in field`.(`<hkobject></hkobject>`)
    ///
    /// # Note
    /// Not present in the binary & Not exist actual C++ field.
    pub __ptr: Option<Pointer>,
    /// Alternative to C++ class inheritance.
    pub parent: hkbSequencedData,
    /// # C++ Info
    /// - name: `samples`(ctype: `hkArray<struct hkbIntVariableSequencedDataSample>`)
    /// - offset: `  8`(x86)/` 16`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    pub m_samples: Vec<hkbIntVariableSequencedDataSample>,
    /// # C++ Info
    /// - name: `variableIndex`(ctype: `hkInt32`)
    /// - offset: ` 20`(x86)/` 32`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    pub m_variableIndex: i32,
}
const _: () = {
    use havok_serde as _serde;
    impl _serde::HavokClass for hkbIntVariableSequencedData {
        #[inline]
        fn name(&self) -> &'static str {
            "hkbIntVariableSequencedData"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0x7bfc518a)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<usize> {
            let mut v = Vec::new();
            v.extend(
                self
                    .m_samples
                    .iter()
                    .flat_map(|class| class.deps_indexes())
                    .collect::<Vec<usize>>(),
            );
            v
        }
    }
    impl _serde::Serialize for hkbIntVariableSequencedData {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .map(|name| (name, _serde::__private::Signature::new(0x7bfc518a)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkbIntVariableSequencedData",
                    class_meta,
                    (24u64, 40u64),
                )?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer
                .skip_field("memSizeAndFlags", &self.parent.parent.m_memSizeAndFlags)?;
            serializer
                .skip_field("referenceCount", &self.parent.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer
                .serialize_array_field(
                    "samples",
                    &self.m_samples,
                    TypeSize::Struct {
                        size_x86: 8u64,
                        size_x86_64: 8u64,
                    },
                )?;
            serializer.serialize_field("variableIndex", &self.m_variableIndex)?;
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
    impl<'de> _serde::Deserialize<'de> for hkbIntVariableSequencedData {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_samples,
                m_variableIndex,
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
                        "samples" => Ok(__Field::m_samples),
                        "variableIndex" => Ok(__Field::m_variableIndex),
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
            struct __hkbIntVariableSequencedDataVisitor<'de> {
                marker: _serde::__private::PhantomData<hkbIntVariableSequencedData>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkbIntVariableSequencedDataVisitor<'de> {
                type Value = hkbIntVariableSequencedData;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkbIntVariableSequencedData",
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
                    let mut m_samples: _serde::__private::Option<
                        Vec<hkbIntVariableSequencedDataSample>,
                    > = _serde::__private::None;
                    let mut m_variableIndex: _serde::__private::Option<i32> = _serde::__private::None;
                    for i in 0..2usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_samples) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "samples",
                                        ),
                                    );
                                }
                                m_samples = _serde::__private::Some(
                                    match __A::next_value::<
                                        Vec<hkbIntVariableSequencedDataSample>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_variableIndex) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "variableIndex",
                                        ),
                                    );
                                }
                                m_variableIndex = _serde::__private::Some(
                                    match __A::next_value::<i32>(&mut __map) {
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
                    let m_samples = match m_samples {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("samples"),
                            );
                        }
                    };
                    let m_variableIndex = match m_variableIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "variableIndex",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkbIntVariableSequencedData {
                        __ptr,
                        parent,
                        m_samples,
                        m_variableIndex,
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
                    let mut m_samples: _serde::__private::Option<
                        Vec<hkbIntVariableSequencedDataSample>,
                    > = _serde::__private::None;
                    let mut m_variableIndex: _serde::__private::Option<i32> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_samples => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_samples) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "samples",
                                        ),
                                    );
                                }
                                m_samples = _serde::__private::Some(
                                    match __A::next_value::<
                                        Vec<hkbIntVariableSequencedDataSample>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_variableIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_variableIndex) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "variableIndex",
                                        ),
                                    );
                                }
                                m_variableIndex = _serde::__private::Some(
                                    match __A::next_value::<i32>(&mut __map) {
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
                    let m_samples = match m_samples {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("samples"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_variableIndex = match m_variableIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "variableIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = None;
                    let parent = hkBaseObject { __ptr };
                    let parent = hkReferencedObject {
                        __ptr,
                        parent,
                        ..Default::default()
                    };
                    let parent = hkbSequencedData { __ptr, parent };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkbIntVariableSequencedData {
                        __ptr,
                        parent,
                        m_samples,
                        m_variableIndex,
                    })
                }
            }
            const FIELDS: &[&str] = &["samples", "variableIndex"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkbIntVariableSequencedData",
                FIELDS,
                __hkbIntVariableSequencedDataVisitor {
                    marker: _serde::__private::PhantomData::<
                        hkbIntVariableSequencedData,
                    >,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
