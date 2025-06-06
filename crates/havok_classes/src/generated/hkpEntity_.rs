use super::class_requires::*;
use super::*;
/// # C++ Info
/// - name: `hkpEntity`
/// - version: `3`
/// - signature: `0xa03c774b`
/// - size: `544`(x86)/`720`(x86_64)
/// -  vtable: `true`
#[allow(non_upper_case_globals, non_snake_case)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(educe::Educe)]
#[educe(Debug, Clone, Default, PartialEq)]
pub struct hkpEntity<'a> {
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
    pub parent: hkpWorldObject<'a>,
    /// # C++ Info
    /// - name: `material`(ctype: `struct hkpMaterial`)
    /// - offset: `140`(x86)/`208`(x86_64)
    /// - type_size: ` 12`(x86)/` 12`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "material"))]
    #[cfg_attr(feature = "serde", serde(rename = "material"))]
    pub m_material: hkpMaterial<'a>,
    /// # C++ Info
    /// - name: `limitContactImpulseUtilAndFlag`(ctype: `void*`)
    /// - offset: `152`(x86)/`224`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(
        feature = "json_schema",
        schemars(rename = "limitContactImpulseUtilAndFlag")
    )]
    #[cfg_attr(feature = "serde", serde(rename = "limitContactImpulseUtilAndFlag"))]
    pub m_limitContactImpulseUtilAndFlag: Pointer<'a>,
    /// # C++ Info
    /// - name: `damageMultiplier`(ctype: `hkReal`)
    /// - offset: `156`(x86)/`232`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "damageMultiplier"))]
    #[cfg_attr(feature = "serde", serde(rename = "damageMultiplier"))]
    pub m_damageMultiplier: f32,
    /// # C++ Info
    /// - name: `breakableBody`(ctype: `void*`)
    /// - offset: `160`(x86)/`240`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "breakableBody"))]
    #[cfg_attr(feature = "serde", serde(rename = "breakableBody"))]
    pub m_breakableBody: Pointer<'a>,
    /// # C++ Info
    /// - name: `solverData`(ctype: `hkUint32`)
    /// - offset: `164`(x86)/`248`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "solverData"))]
    #[cfg_attr(feature = "serde", serde(rename = "solverData"))]
    pub m_solverData: U32<'a>,
    /// # C++ Info
    /// - name: `storageIndex`(ctype: `hkUint16`)
    /// - offset: `168`(x86)/`252`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "storageIndex"))]
    #[cfg_attr(feature = "serde", serde(rename = "storageIndex"))]
    pub m_storageIndex: U16<'a>,
    /// # C++ Info
    /// - name: `contactPointCallbackDelay`(ctype: `hkUint16`)
    /// - offset: `170`(x86)/`254`(x86_64)
    /// - type_size: `  2`(x86)/`  2`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "contactPointCallbackDelay"))]
    #[cfg_attr(feature = "serde", serde(rename = "contactPointCallbackDelay"))]
    pub m_contactPointCallbackDelay: U16<'a>,
    /// # C++ Info
    /// - name: `constraintsMaster`(ctype: `struct hkpEntitySmallArraySerializeOverrideType`)
    /// - offset: `172`(x86)/`256`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "constraintsMaster"))]
    #[cfg_attr(feature = "serde", serde(rename = "constraintsMaster"))]
    pub m_constraintsMaster: hkpEntitySmallArraySerializeOverrideType<'a>,
    /// # C++ Info
    /// - name: `constraintsSlave`(ctype: `hkArray<hkpConstraintInstance*>`)
    /// - offset: `180`(x86)/`272`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    /// - flags: `NOT_OWNED|SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "constraintsSlave"))]
    #[cfg_attr(feature = "serde", serde(rename = "constraintsSlave"))]
    pub m_constraintsSlave: Vec<Pointer<'a>>,
    /// # C++ Info
    /// - name: `constraintRuntime`(ctype: `hkArray<hkUint8>`)
    /// - offset: `192`(x86)/`288`(x86_64)
    /// - type_size: ` 12`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "constraintRuntime"))]
    #[cfg_attr(feature = "serde", serde(rename = "constraintRuntime"))]
    pub m_constraintRuntime: Vec<U8<'a>>,
    /// # C++ Info
    /// - name: `simulationIsland`(ctype: `void*`)
    /// - offset: `204`(x86)/`304`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "simulationIsland"))]
    #[cfg_attr(feature = "serde", serde(rename = "simulationIsland"))]
    pub m_simulationIsland: Pointer<'a>,
    /// # C++ Info
    /// - name: `autoRemoveLevel`(ctype: `hkInt8`)
    /// - offset: `208`(x86)/`312`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "autoRemoveLevel"))]
    #[cfg_attr(feature = "serde", serde(rename = "autoRemoveLevel"))]
    pub m_autoRemoveLevel: I8<'a>,
    /// # C++ Info
    /// - name: `numShapeKeysInContactPointProperties`(ctype: `hkUint8`)
    /// - offset: `209`(x86)/`313`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(
        feature = "json_schema",
        schemars(rename = "numShapeKeysInContactPointProperties")
    )]
    #[cfg_attr(
        feature = "serde",
        serde(rename = "numShapeKeysInContactPointProperties")
    )]
    pub m_numShapeKeysInContactPointProperties: U8<'a>,
    /// # C++ Info
    /// - name: `responseModifierFlags`(ctype: `hkUint8`)
    /// - offset: `210`(x86)/`314`(x86_64)
    /// - type_size: `  1`(x86)/`  1`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "responseModifierFlags"))]
    #[cfg_attr(feature = "serde", serde(rename = "responseModifierFlags"))]
    pub m_responseModifierFlags: U8<'a>,
    /// # C++ Info
    /// - name: `uid`(ctype: `hkUint32`)
    /// - offset: `212`(x86)/`316`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "uid"))]
    #[cfg_attr(feature = "serde", serde(rename = "uid"))]
    pub m_uid: U32<'a>,
    /// # C++ Info
    /// - name: `spuCollisionCallback`(ctype: `struct hkpEntitySpuCollisionCallback`)
    /// - offset: `216`(x86)/`320`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "spuCollisionCallback"))]
    #[cfg_attr(feature = "serde", serde(rename = "spuCollisionCallback"))]
    pub m_spuCollisionCallback: hkpEntitySpuCollisionCallback<'a>,
    /// # C++ Info
    /// - name: `motion`(ctype: `struct hkpMaxSizeMotion`)
    /// - offset: `224`(x86)/`336`(x86_64)
    /// - type_size: `288`(x86)/`320`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "motion"))]
    #[cfg_attr(feature = "serde", serde(rename = "motion"))]
    pub m_motion: hkpMaxSizeMotion<'a>,
    /// # C++ Info
    /// - name: `contactListeners`(ctype: `struct hkpEntitySmallArraySerializeOverrideType`)
    /// - offset: `512`(x86)/`656`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "contactListeners"))]
    #[cfg_attr(feature = "serde", serde(rename = "contactListeners"))]
    pub m_contactListeners: hkpEntitySmallArraySerializeOverrideType<'a>,
    /// # C++ Info
    /// - name: `actions`(ctype: `struct hkpEntitySmallArraySerializeOverrideType`)
    /// - offset: `520`(x86)/`672`(x86_64)
    /// - type_size: `  8`(x86)/` 16`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "actions"))]
    #[cfg_attr(feature = "serde", serde(rename = "actions"))]
    pub m_actions: hkpEntitySmallArraySerializeOverrideType<'a>,
    /// # C++ Info
    /// - name: `localFrame`(ctype: `struct hkLocalFrame*`)
    /// - offset: `528`(x86)/`688`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "localFrame"))]
    #[cfg_attr(feature = "serde", serde(rename = "localFrame"))]
    pub m_localFrame: Pointer<'a>,
    /// # C++ Info
    /// - name: `extendedListeners`(ctype: `struct hkpEntityExtendedListeners*`)
    /// - offset: `532`(x86)/`696`(x86_64)
    /// - type_size: `  4`(x86)/`  8`(x86_64)
    /// - flags: `SERIALIZE_IGNORED`
    #[cfg_attr(feature = "serde", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "extendedListeners"))]
    #[cfg_attr(feature = "serde", serde(rename = "extendedListeners"))]
    pub m_extendedListeners: Pointer<'a>,
    /// # C++ Info
    /// - name: `npData`(ctype: `hkUint32`)
    /// - offset: `536`(x86)/`704`(x86_64)
    /// - type_size: `  4`(x86)/`  4`(x86_64)
    #[cfg_attr(feature = "serde_default", serde(default))]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "json_schema", schemars(rename = "npData"))]
    #[cfg_attr(feature = "serde", serde(rename = "npData"))]
    pub m_npData: U32<'a>,
}
const _: () = {
    use havok_serde as _serde;
    impl<'a> _serde::HavokClass for hkpEntity<'a> {
        #[inline]
        fn name(&self) -> &'static str {
            "hkpEntity"
        }
        #[inline]
        fn signature(&self) -> _serde::__private::Signature {
            _serde::__private::Signature::new(0xa03c774b)
        }
        #[allow(clippy::let_and_return, clippy::vec_init_then_push)]
        fn deps_indexes(&self) -> Vec<&Pointer<'_>> {
            let mut v = Vec::new();
            v.push(&self.parent.m_world);
            v.extend(self.parent.m_collidable.deps_indexes());
            v.extend(self.parent.m_multiThreadCheck.deps_indexes());
            v.extend(
                self
                    .parent
                    .m_properties
                    .iter()
                    .flat_map(|class| class.deps_indexes())
                    .collect::<Vec<&Pointer<'_>>>(),
            );
            v.push(&self.parent.m_treeData);
            v.extend(self.m_material.deps_indexes());
            v.push(&self.m_limitContactImpulseUtilAndFlag);
            v.push(&self.m_breakableBody);
            v.extend(self.m_constraintsMaster.deps_indexes());
            v.extend(self.m_constraintsSlave.iter());
            v.push(&self.m_simulationIsland);
            v.extend(self.m_spuCollisionCallback.deps_indexes());
            v.extend(self.m_motion.deps_indexes());
            v.extend(self.m_contactListeners.deps_indexes());
            v.extend(self.m_actions.deps_indexes());
            v.push(&self.m_localFrame);
            v.push(&self.m_extendedListeners);
            v
        }
    }
    impl<'a> _serde::Serialize for hkpEntity<'a> {
        fn serialize<S>(&self, __serializer: S) -> Result<S::Ok, S::Error>
        where
            S: _serde::ser::Serializer,
        {
            let class_meta = self
                .__ptr
                .as_ref()
                .map(|name| (name, _serde::__private::Signature::new(0xa03c774b)));
            let mut serializer = __serializer
                .serialize_struct("hkpEntity", class_meta, (544u64, 720u64))?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 8usize].as_slice())?;
            serializer
                .skip_field("memSizeAndFlags", &self.parent.parent.m_memSizeAndFlags)?;
            serializer
                .skip_field("referenceCount", &self.parent.parent.m_referenceCount)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.skip_field("world", &self.parent.m_world)?;
            serializer.serialize_field("userData", &self.parent.m_userData)?;
            serializer.serialize_field("collidable", &self.parent.m_collidable)?;
            serializer
                .serialize_field("multiThreadCheck", &self.parent.m_multiThreadCheck)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.serialize_field("name", &self.parent.m_name)?;
            serializer
                .serialize_array_field(
                    "properties",
                    &self.parent.m_properties,
                    TypeSize::Struct {
                        size_x86: 16u64,
                        size_x86_64: 16u64,
                    },
                )?;
            serializer.skip_field("treeData", &self.parent.m_treeData)?;
            serializer.serialize_field("material", &self.m_material)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer
                .skip_field(
                    "limitContactImpulseUtilAndFlag",
                    &self.m_limitContactImpulseUtilAndFlag,
                )?;
            serializer.serialize_field("damageMultiplier", &self.m_damageMultiplier)?;
            serializer.pad_field([0u8; 0usize].as_slice(), [0u8; 4usize].as_slice())?;
            serializer.skip_field("breakableBody", &self.m_breakableBody)?;
            serializer.skip_field("solverData", &self.m_solverData)?;
            serializer.serialize_field("storageIndex", &self.m_storageIndex)?;
            serializer
                .serialize_field(
                    "contactPointCallbackDelay",
                    &self.m_contactPointCallbackDelay,
                )?;
            serializer.skip_field("constraintsMaster", &self.m_constraintsMaster)?;
            serializer
                .skip_array_field(
                    "constraintsSlave",
                    &self.m_constraintsSlave,
                    TypeSize::NonPtr,
                )?;
            serializer
                .skip_array_field(
                    "constraintRuntime",
                    &self.m_constraintRuntime,
                    TypeSize::NonPtr,
                )?;
            serializer.skip_field("simulationIsland", &self.m_simulationIsland)?;
            serializer.serialize_field("autoRemoveLevel", &self.m_autoRemoveLevel)?;
            serializer
                .serialize_field(
                    "numShapeKeysInContactPointProperties",
                    &self.m_numShapeKeysInContactPointProperties,
                )?;
            serializer
                .serialize_field(
                    "responseModifierFlags",
                    &self.m_responseModifierFlags,
                )?;
            serializer.pad_field([0u8; 1usize].as_slice(), [0u8; 1usize].as_slice())?;
            serializer.serialize_field("uid", &self.m_uid)?;
            serializer
                .serialize_field("spuCollisionCallback", &self.m_spuCollisionCallback)?;
            serializer.serialize_field("motion", &self.m_motion)?;
            serializer.skip_field("contactListeners", &self.m_contactListeners)?;
            serializer.skip_field("actions", &self.m_actions)?;
            serializer.serialize_field("localFrame", &self.m_localFrame)?;
            serializer.skip_field("extendedListeners", &self.m_extendedListeners)?;
            serializer.serialize_field("npData", &self.m_npData)?;
            serializer.pad_field([0u8; 4usize].as_slice(), [0u8; 12usize].as_slice())?;
            serializer.end()
        }
    }
};
#[doc(hidden)]
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _: () = {
    use havok_serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for hkpEntity<'de> {
        fn deserialize<__D>(deserializer: __D) -> core::result::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                m_userData,
                m_collidable,
                m_multiThreadCheck,
                m_name,
                m_properties,
                m_material,
                m_damageMultiplier,
                m_storageIndex,
                m_contactPointCallbackDelay,
                m_autoRemoveLevel,
                m_numShapeKeysInContactPointProperties,
                m_responseModifierFlags,
                m_uid,
                m_spuCollisionCallback,
                m_motion,
                m_localFrame,
                m_npData,
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
                        "userData" => Ok(__Field::m_userData),
                        "collidable" => Ok(__Field::m_collidable),
                        "multiThreadCheck" => Ok(__Field::m_multiThreadCheck),
                        "name" => Ok(__Field::m_name),
                        "properties" => Ok(__Field::m_properties),
                        "material" => Ok(__Field::m_material),
                        "damageMultiplier" => Ok(__Field::m_damageMultiplier),
                        "storageIndex" => Ok(__Field::m_storageIndex),
                        "contactPointCallbackDelay" => {
                            Ok(__Field::m_contactPointCallbackDelay)
                        }
                        "autoRemoveLevel" => Ok(__Field::m_autoRemoveLevel),
                        "numShapeKeysInContactPointProperties" => {
                            Ok(__Field::m_numShapeKeysInContactPointProperties)
                        }
                        "responseModifierFlags" => Ok(__Field::m_responseModifierFlags),
                        "uid" => Ok(__Field::m_uid),
                        "spuCollisionCallback" => Ok(__Field::m_spuCollisionCallback),
                        "motion" => Ok(__Field::m_motion),
                        "localFrame" => Ok(__Field::m_localFrame),
                        "npData" => Ok(__Field::m_npData),
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
            struct __hkpEntityVisitor<'de> {
                marker: _serde::__private::PhantomData<hkpEntity<'de>>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::reversed_empty_ranges)]
            #[allow(clippy::single_match)]
            impl<'de> _serde::de::Visitor<'de> for __hkpEntityVisitor<'de> {
                type Value = hkpEntity<'de>;
                fn expecting(
                    &self,
                    __formatter: &mut core::fmt::Formatter,
                ) -> core::fmt::Result {
                    core::fmt::Formatter::write_str(__formatter, "struct hkpEntity")
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
                    let mut m_material: _serde::__private::Option<hkpMaterial<'de>> = _serde::__private::None;
                    let mut m_limitContactImpulseUtilAndFlag: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_damageMultiplier: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_breakableBody: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_solverData: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_storageIndex: _serde::__private::Option<U16<'de>> = _serde::__private::None;
                    let mut m_contactPointCallbackDelay: _serde::__private::Option<
                        U16<'de>,
                    > = _serde::__private::None;
                    let mut m_constraintsMaster: _serde::__private::Option<
                        hkpEntitySmallArraySerializeOverrideType<'de>,
                    > = _serde::__private::None;
                    let mut m_constraintsSlave: _serde::__private::Option<
                        Vec<Pointer<'de>>,
                    > = _serde::__private::None;
                    let mut m_constraintRuntime: _serde::__private::Option<
                        Vec<U8<'de>>,
                    > = _serde::__private::None;
                    let mut m_simulationIsland: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_autoRemoveLevel: _serde::__private::Option<I8<'de>> = _serde::__private::None;
                    let mut m_numShapeKeysInContactPointProperties: _serde::__private::Option<
                        U8<'de>,
                    > = _serde::__private::None;
                    let mut m_responseModifierFlags: _serde::__private::Option<
                        U8<'de>,
                    > = _serde::__private::None;
                    let mut m_uid: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_spuCollisionCallback: _serde::__private::Option<
                        hkpEntitySpuCollisionCallback<'de>,
                    > = _serde::__private::None;
                    let mut m_motion: _serde::__private::Option<hkpMaxSizeMotion<'de>> = _serde::__private::None;
                    let mut m_contactListeners: _serde::__private::Option<
                        hkpEntitySmallArraySerializeOverrideType<'de>,
                    > = _serde::__private::None;
                    let mut m_actions: _serde::__private::Option<
                        hkpEntitySmallArraySerializeOverrideType<'de>,
                    > = _serde::__private::None;
                    let mut m_localFrame: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_extendedListeners: _serde::__private::Option<
                        Pointer<'de>,
                    > = _serde::__private::None;
                    let mut m_npData: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    for i in 0..22usize {
                        match i {
                            0usize => {
                                if _serde::__private::Option::is_some(&m_material) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "material",
                                        ),
                                    );
                                }
                                m_material = _serde::__private::Some(
                                    match __A::next_value::<hkpMaterial<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            1usize => {
                                if _serde::__private::Option::is_some(
                                    &m_limitContactImpulseUtilAndFlag,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "limitContactImpulseUtilAndFlag",
                                        ),
                                    );
                                }
                                __A::pad(&mut __map, 0usize, 4usize)?;
                                m_limitContactImpulseUtilAndFlag = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            2usize => {
                                if _serde::__private::Option::is_some(&m_damageMultiplier) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "damageMultiplier",
                                        ),
                                    );
                                }
                                m_damageMultiplier = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            3usize => {
                                if _serde::__private::Option::is_some(&m_breakableBody) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "breakableBody",
                                        ),
                                    );
                                }
                                __A::pad(&mut __map, 0usize, 4usize)?;
                                m_breakableBody = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            4usize => {
                                if _serde::__private::Option::is_some(&m_solverData) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "solverData",
                                        ),
                                    );
                                }
                                m_solverData = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            5usize => {
                                if _serde::__private::Option::is_some(&m_storageIndex) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "storageIndex",
                                        ),
                                    );
                                }
                                m_storageIndex = _serde::__private::Some(
                                    match __A::next_value::<U16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            6usize => {
                                if _serde::__private::Option::is_some(
                                    &m_contactPointCallbackDelay,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "contactPointCallbackDelay",
                                        ),
                                    );
                                }
                                m_contactPointCallbackDelay = _serde::__private::Some(
                                    match __A::next_value::<U16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            7usize => {
                                if _serde::__private::Option::is_some(
                                    &m_constraintsMaster,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "constraintsMaster",
                                        ),
                                    );
                                }
                                m_constraintsMaster = _serde::__private::Some(
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
                            8usize => {
                                if _serde::__private::Option::is_some(&m_constraintsSlave) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "constraintsSlave",
                                        ),
                                    );
                                }
                                m_constraintsSlave = _serde::__private::Some(
                                    match __A::next_value::<Vec<Pointer<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            9usize => {
                                if _serde::__private::Option::is_some(
                                    &m_constraintRuntime,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "constraintRuntime",
                                        ),
                                    );
                                }
                                m_constraintRuntime = _serde::__private::Some(
                                    match __A::next_value::<Vec<U8<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            10usize => {
                                if _serde::__private::Option::is_some(&m_simulationIsland) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "simulationIsland",
                                        ),
                                    );
                                }
                                m_simulationIsland = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            11usize => {
                                if _serde::__private::Option::is_some(&m_autoRemoveLevel) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "autoRemoveLevel",
                                        ),
                                    );
                                }
                                m_autoRemoveLevel = _serde::__private::Some(
                                    match __A::next_value::<I8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            12usize => {
                                if _serde::__private::Option::is_some(
                                    &m_numShapeKeysInContactPointProperties,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numShapeKeysInContactPointProperties",
                                        ),
                                    );
                                }
                                m_numShapeKeysInContactPointProperties = _serde::__private::Some(
                                    match __A::next_value::<U8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            13usize => {
                                if _serde::__private::Option::is_some(
                                    &m_responseModifierFlags,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "responseModifierFlags",
                                        ),
                                    );
                                }
                                m_responseModifierFlags = _serde::__private::Some(
                                    match __A::next_value::<U8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            14usize => {
                                if _serde::__private::Option::is_some(&m_uid) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("uid"),
                                    );
                                }
                                __A::pad(&mut __map, 1usize, 1usize)?;
                                m_uid = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            15usize => {
                                if _serde::__private::Option::is_some(
                                    &m_spuCollisionCallback,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "spuCollisionCallback",
                                        ),
                                    );
                                }
                                m_spuCollisionCallback = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpEntitySpuCollisionCallback<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            16usize => {
                                if _serde::__private::Option::is_some(&m_motion) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("motion"),
                                    );
                                }
                                m_motion = _serde::__private::Some(
                                    match __A::next_value::<hkpMaxSizeMotion<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            17usize => {
                                if _serde::__private::Option::is_some(&m_contactListeners) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "contactListeners",
                                        ),
                                    );
                                }
                                m_contactListeners = _serde::__private::Some(
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
                            18usize => {
                                if _serde::__private::Option::is_some(&m_actions) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "actions",
                                        ),
                                    );
                                }
                                m_actions = _serde::__private::Some(
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
                            19usize => {
                                if _serde::__private::Option::is_some(&m_localFrame) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "localFrame",
                                        ),
                                    );
                                }
                                m_localFrame = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            20usize => {
                                if _serde::__private::Option::is_some(
                                    &m_extendedListeners,
                                ) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "extendedListeners",
                                        ),
                                    );
                                }
                                m_extendedListeners = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            21usize => {
                                if _serde::__private::Option::is_some(&m_npData) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("npData"),
                                    );
                                }
                                m_npData = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
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
                    __A::pad(&mut __map, 4usize, 12usize)?;
                    let m_material = match m_material {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("material"),
                            );
                        }
                    };
                    let m_limitContactImpulseUtilAndFlag = match m_limitContactImpulseUtilAndFlag {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "limitContactImpulseUtilAndFlag",
                                ),
                            );
                        }
                    };
                    let m_damageMultiplier = match m_damageMultiplier {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "damageMultiplier",
                                ),
                            );
                        }
                    };
                    let m_breakableBody = match m_breakableBody {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "breakableBody",
                                ),
                            );
                        }
                    };
                    let m_solverData = match m_solverData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "solverData",
                                ),
                            );
                        }
                    };
                    let m_storageIndex = match m_storageIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "storageIndex",
                                ),
                            );
                        }
                    };
                    let m_contactPointCallbackDelay = match m_contactPointCallbackDelay {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "contactPointCallbackDelay",
                                ),
                            );
                        }
                    };
                    let m_constraintsMaster = match m_constraintsMaster {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "constraintsMaster",
                                ),
                            );
                        }
                    };
                    let m_constraintsSlave = match m_constraintsSlave {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "constraintsSlave",
                                ),
                            );
                        }
                    };
                    let m_constraintRuntime = match m_constraintRuntime {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "constraintRuntime",
                                ),
                            );
                        }
                    };
                    let m_simulationIsland = match m_simulationIsland {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "simulationIsland",
                                ),
                            );
                        }
                    };
                    let m_autoRemoveLevel = match m_autoRemoveLevel {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "autoRemoveLevel",
                                ),
                            );
                        }
                    };
                    let m_numShapeKeysInContactPointProperties = match m_numShapeKeysInContactPointProperties {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numShapeKeysInContactPointProperties",
                                ),
                            );
                        }
                    };
                    let m_responseModifierFlags = match m_responseModifierFlags {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "responseModifierFlags",
                                ),
                            );
                        }
                    };
                    let m_uid = match m_uid {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("uid"),
                            );
                        }
                    };
                    let m_spuCollisionCallback = match m_spuCollisionCallback {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "spuCollisionCallback",
                                ),
                            );
                        }
                    };
                    let m_motion = match m_motion {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("motion"),
                            );
                        }
                    };
                    let m_contactListeners = match m_contactListeners {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "contactListeners",
                                ),
                            );
                        }
                    };
                    let m_actions = match m_actions {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("actions"),
                            );
                        }
                    };
                    let m_localFrame = match m_localFrame {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "localFrame",
                                ),
                            );
                        }
                    };
                    let m_extendedListeners = match m_extendedListeners {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "extendedListeners",
                                ),
                            );
                        }
                    };
                    let m_npData = match m_npData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("npData"),
                            );
                        }
                    };
                    _serde::__private::Ok(hkpEntity {
                        __ptr,
                        parent,
                        m_material,
                        m_limitContactImpulseUtilAndFlag,
                        m_damageMultiplier,
                        m_breakableBody,
                        m_solverData,
                        m_storageIndex,
                        m_contactPointCallbackDelay,
                        m_constraintsMaster,
                        m_constraintsSlave,
                        m_constraintRuntime,
                        m_simulationIsland,
                        m_autoRemoveLevel,
                        m_numShapeKeysInContactPointProperties,
                        m_responseModifierFlags,
                        m_uid,
                        m_spuCollisionCallback,
                        m_motion,
                        m_contactListeners,
                        m_actions,
                        m_localFrame,
                        m_extendedListeners,
                        m_npData,
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
                    let mut m_userData: _serde::__private::Option<Ulong> = _serde::__private::None;
                    let mut m_collidable: _serde::__private::Option<
                        hkpLinkedCollidable<'de>,
                    > = _serde::__private::None;
                    let mut m_multiThreadCheck: _serde::__private::Option<
                        hkMultiThreadCheck<'de>,
                    > = _serde::__private::None;
                    let mut m_name: _serde::__private::Option<StringPtr<'de>> = _serde::__private::None;
                    let mut m_properties: _serde::__private::Option<
                        Vec<hkpProperty<'de>>,
                    > = _serde::__private::None;
                    let mut m_material: _serde::__private::Option<hkpMaterial<'de>> = _serde::__private::None;
                    let mut m_damageMultiplier: _serde::__private::Option<f32> = _serde::__private::None;
                    let mut m_storageIndex: _serde::__private::Option<U16<'de>> = _serde::__private::None;
                    let mut m_contactPointCallbackDelay: _serde::__private::Option<
                        U16<'de>,
                    > = _serde::__private::None;
                    let mut m_autoRemoveLevel: _serde::__private::Option<I8<'de>> = _serde::__private::None;
                    let mut m_numShapeKeysInContactPointProperties: _serde::__private::Option<
                        U8<'de>,
                    > = _serde::__private::None;
                    let mut m_responseModifierFlags: _serde::__private::Option<
                        U8<'de>,
                    > = _serde::__private::None;
                    let mut m_uid: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    let mut m_spuCollisionCallback: _serde::__private::Option<
                        hkpEntitySpuCollisionCallback<'de>,
                    > = _serde::__private::None;
                    let mut m_motion: _serde::__private::Option<hkpMaxSizeMotion<'de>> = _serde::__private::None;
                    let mut m_localFrame: _serde::__private::Option<Pointer<'de>> = _serde::__private::None;
                    let mut m_npData: _serde::__private::Option<U32<'de>> = _serde::__private::None;
                    while let _serde::__private::Some(__key) = {
                        __A::next_key::<__Field>(&mut __map)?
                    } {
                        match __key {
                            __Field::m_userData => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_userData) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "userData",
                                        ),
                                    );
                                }
                                m_userData = _serde::__private::Some(
                                    match __A::next_value::<Ulong>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_collidable => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_collidable) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "collidable",
                                        ),
                                    );
                                }
                                m_collidable = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpLinkedCollidable<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_multiThreadCheck => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_multiThreadCheck) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "multiThreadCheck",
                                        ),
                                    );
                                }
                                m_multiThreadCheck = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkMultiThreadCheck<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_name => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_name) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("name"),
                                    );
                                }
                                m_name = _serde::__private::Some(
                                    match __A::next_value::<StringPtr<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_properties => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_properties) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "properties",
                                        ),
                                    );
                                }
                                m_properties = _serde::__private::Some(
                                    match __A::next_value::<Vec<hkpProperty<'de>>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_material => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_material) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "material",
                                        ),
                                    );
                                }
                                m_material = _serde::__private::Some(
                                    match __A::next_value::<hkpMaterial<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_damageMultiplier => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_damageMultiplier) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "damageMultiplier",
                                        ),
                                    );
                                }
                                m_damageMultiplier = _serde::__private::Some(
                                    match __A::next_value::<f32>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_storageIndex => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_storageIndex) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "storageIndex",
                                        ),
                                    );
                                }
                                m_storageIndex = _serde::__private::Some(
                                    match __A::next_value::<U16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_contactPointCallbackDelay => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_contactPointCallbackDelay,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "contactPointCallbackDelay",
                                        ),
                                    );
                                }
                                m_contactPointCallbackDelay = _serde::__private::Some(
                                    match __A::next_value::<U16<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_autoRemoveLevel => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_autoRemoveLevel) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "autoRemoveLevel",
                                        ),
                                    );
                                }
                                m_autoRemoveLevel = _serde::__private::Some(
                                    match __A::next_value::<I8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_numShapeKeysInContactPointProperties => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_numShapeKeysInContactPointProperties,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "numShapeKeysInContactPointProperties",
                                        ),
                                    );
                                }
                                m_numShapeKeysInContactPointProperties = _serde::__private::Some(
                                    match __A::next_value::<U8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_responseModifierFlags => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_responseModifierFlags,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "responseModifierFlags",
                                        ),
                                    );
                                }
                                m_responseModifierFlags = _serde::__private::Some(
                                    match __A::next_value::<U8<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_uid => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_uid) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("uid"),
                                    );
                                }
                                m_uid = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_spuCollisionCallback => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(
                                    &m_spuCollisionCallback,
                                ) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "spuCollisionCallback",
                                        ),
                                    );
                                }
                                m_spuCollisionCallback = _serde::__private::Some(
                                    match __A::next_value::<
                                        hkpEntitySpuCollisionCallback<'de>,
                                    >(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_motion => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_motion) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("motion"),
                                    );
                                }
                                m_motion = _serde::__private::Some(
                                    match __A::next_value::<hkpMaxSizeMotion<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_localFrame => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_localFrame) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "localFrame",
                                        ),
                                    );
                                }
                                m_localFrame = _serde::__private::Some(
                                    match __A::next_value::<Pointer<'de>>(&mut __map) {
                                        _serde::__private::Ok(__val) => __val,
                                        _serde::__private::Err(__err) => {
                                            return _serde::__private::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::m_npData => {
                                #[cfg(
                                    any(feature = "strict", feature = "ignore_duplicates")
                                )]
                                if _serde::__private::Option::is_some(&m_npData) {
                                    #[cfg(feature = "ignore_duplicates")]
                                    {
                                        __A::skip_value(&mut __map)?;
                                        continue;
                                    }
                                    #[cfg(feature = "strict")]
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("npData"),
                                    );
                                }
                                m_npData = _serde::__private::Some(
                                    match __A::next_value::<U32<'de>>(&mut __map) {
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
                    let m_userData = match m_userData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("userData"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_collidable = match m_collidable {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "collidable",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_multiThreadCheck = match m_multiThreadCheck {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "multiThreadCheck",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_name = match m_name {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("name"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_properties = match m_properties {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "properties",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_material = match m_material {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("material"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_damageMultiplier = match m_damageMultiplier {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "damageMultiplier",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_storageIndex = match m_storageIndex {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "storageIndex",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_contactPointCallbackDelay = match m_contactPointCallbackDelay {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "contactPointCallbackDelay",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_autoRemoveLevel = match m_autoRemoveLevel {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "autoRemoveLevel",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_numShapeKeysInContactPointProperties = match m_numShapeKeysInContactPointProperties {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "numShapeKeysInContactPointProperties",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_responseModifierFlags = match m_responseModifierFlags {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "responseModifierFlags",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_uid = match m_uid {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("uid"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_spuCollisionCallback = match m_spuCollisionCallback {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "spuCollisionCallback",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_motion = match m_motion {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("motion"),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_localFrame = match m_localFrame {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field(
                                    "localFrame",
                                ),
                            );
                            #[cfg(not(feature = "strict"))] Default::default()
                        }
                    };
                    let m_npData = match m_npData {
                        _serde::__private::Some(__field) => __field,
                        _serde::__private::None => {
                            #[cfg(feature = "strict")]
                            return _serde::__private::Err(
                                <__A::Error as _serde::de::Error>::missing_field("npData"),
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
                    let parent = hkpWorldObject {
                        __ptr: __ptr.clone(),
                        parent,
                        m_userData,
                        m_collidable,
                        m_multiThreadCheck,
                        m_name,
                        m_properties,
                        ..Default::default()
                    };
                    let __ptr = __A::class_ptr(&mut __map);
                    _serde::__private::Ok(hkpEntity {
                        __ptr: __ptr.clone(),
                        parent,
                        m_material,
                        m_damageMultiplier,
                        m_storageIndex,
                        m_contactPointCallbackDelay,
                        m_autoRemoveLevel,
                        m_numShapeKeysInContactPointProperties,
                        m_responseModifierFlags,
                        m_uid,
                        m_spuCollisionCallback,
                        m_motion,
                        m_localFrame,
                        m_npData,
                        ..Default::default()
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "material",
                "limitContactImpulseUtilAndFlag",
                "damageMultiplier",
                "breakableBody",
                "solverData",
                "storageIndex",
                "contactPointCallbackDelay",
                "constraintsMaster",
                "constraintsSlave",
                "constraintRuntime",
                "simulationIsland",
                "autoRemoveLevel",
                "numShapeKeysInContactPointProperties",
                "responseModifierFlags",
                "uid",
                "spuCollisionCallback",
                "motion",
                "contactListeners",
                "actions",
                "localFrame",
                "extendedListeners",
                "npData",
            ];
            _serde::Deserializer::deserialize_struct(
                deserializer,
                "hkpEntity",
                FIELDS,
                __hkpEntityVisitor {
                    marker: _serde::__private::PhantomData::<hkpEntity>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};
