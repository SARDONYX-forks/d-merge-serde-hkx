use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkxMaterialTextureStage`
/// - version: `0`
/// - signature: `0xfa6facb2`
/// - size: ` 12`(x86)/` 16`(x86_64)
/// -  vtable: `false`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkxMaterialTextureStage<'a> {
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
    /// - name: `texture`(ctype: `struct hkReferencedObject*`)
    /// - offset: `  0`(x86)/`  0`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "texture"))]
    #[cfg_attr(feature = "serde", serde(rename = "texture"))]
    pub m_texture: Pointer<'a>,
    /// # C++ Info
    /// - name: `usageHint`(ctype: `enum TextureType`)
    /// - offset: `  4`(x86)/`  8`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "usageHint"))]
    #[cfg_attr(feature = "serde", serde(rename = "usageHint"))]
    pub m_usageHint: TextureType,
    /// # C++ Info
    /// - name: `tcoordChannel`(ctype: `hkInt32`)
    /// - offset: `  8`(x86)/` 12`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "json_schema", schemars(rename = "tcoordChannel"))]
    #[cfg_attr(feature = "serde", serde(rename = "tcoordChannel"))]
    pub m_tcoordChannel: I32<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkxMaterialTextureStage<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkxMaterialTextureStage"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xfa6facb2)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.push(&self.m_texture);
            v
        }
    }
    impl<'a> _serde::Serialize for hkxMaterialTextureStage<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xfa6facb2)));
            let mut serializer = __serializer
                .serialize_struct(
                    "hkxMaterialTextureStage",
                    class_meta,
                    (12u64, 16u64),
                )?;
            serializer.serialize_field("texture", &self.m_texture)?;
            serializer.serialize_field("usageHint", &self.m_usageHint)?;
            serializer.serialize_field("tcoordChannel", &self.m_tcoordChannel)?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkxMaterialTextureStage<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_texture,
                m_usageHint,
                m_tcoordChannel,
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
                        "texture" => Ok(__Field::m_texture),
                        "usageHint" => Ok(__Field::m_usageHint),
                        "tcoordChannel" => Ok(__Field::m_tcoordChannel),
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
            struct __hkxMaterialTextureStageVisitor<'de> {
                marker: _serde::__private::PhantomData<hkxMaterialTextureStage<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de>
            for __hkxMaterialTextureStageVisitor<'de> {
                type Value = hkxMaterialTextureStage<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(
                        __formatter,
                        "struct hkxMaterialTextureStage",
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
                    let mut m_texture: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_usageHint: _serde::__private::Option<TextureType> = _serde::__private::None;
                    let mut m_tcoordChannel: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    for i in 0..3usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_texture) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "texture",
                                        ),
                                    );
                                }
                                m_texture = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(&m_usageHint) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "usageHint",
                                        ),
                                    );
                                }
                                m_usageHint = _serde::__private::Some(
                                    match __A::next_value::<TextureType>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_tcoordChannel) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "tcoordChannel",
                                        ),
                                    );
                                }
                                m_tcoordChannel = _serde::__private::Some(
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
                    let m_texture = match m_texture {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("texture"),
                            );
                        }
                    };
                    let m_usageHint = match m_usageHint {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "usageHint",
                                ),
                            );
                        }
                    };
                    let m_tcoordChannel = match m_tcoordChannel {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "tcoordChannel",
                                ),
                            );
                        }
                    };
                    _serde::__private::Ok(hkxMaterialTextureStage {
                        __ptr,
                        m_texture,
                        m_usageHint,
                        m_tcoordChannel,
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
                    let mut m_texture: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_usageHint: _serde::__private::Option<TextureType> = _serde::__private::None;
                    let mut m_tcoordChannel: _serde::__private::Option<I32<'de>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_texture => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_texture) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "texture",
                                        ),
                                    );
                                }
                                m_texture = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_usageHint => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_usageHint) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "usageHint",
                                        ),
                                    );
                                }
                                m_usageHint = _serde::__private::Some(
                                    match __A::next_value::<TextureType>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_tcoordChannel => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_tcoordChannel) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "tcoordChannel",
                                        ),
                                    );
                                }
                                m_tcoordChannel = _serde::__private::Some(
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
                    let m_texture = match m_texture {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("texture"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_usageHint = match m_usageHint {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "usageHint",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_tcoordChannel = match m_tcoordChannel {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "tcoordChannel",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkxMaterialTextureStage {
                        __ptr: __ptr.clone(),
                        m_texture,
                        m_usageHint,
                        m_tcoordChannel,
                    })
                }
            }
            const FIELDS: &[&str] = &["texture", "usageHint", "tcoordChannel"];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkxMaterialTextureStage",
                FIELDS,
                __hkxMaterialTextureStageVisitor {
                    marker: _serde::__private::PhantomData::<hkxMaterialTextureStage>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
