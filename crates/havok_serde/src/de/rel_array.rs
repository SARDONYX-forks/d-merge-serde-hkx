//! `hkRelArray<T>`(hk2014~) deserialization helpers.
use crate::de::{DeserializeSeed, Deserializer, ReadEnumSize, Visitor};
use crate::lib::*;

/// `hkRelArray<T>` seed.
///
/// Deserialize `Vec<T>` by [`Deserializer::deserialize_rel_array`] instead of [`Deserializer::deserialize_array`].
///
/// # Example(generated code)
/// ```ignore
/// m_vertices = Some(__A::next_value_seed(&mut __map, _serde::de::RelArraySeed::<_>::new())?);
/// ```
pub struct RelArraySeed<T>(PhantomData<T>);

impl<T> RelArraySeed<T> {
    /// Create a new `hkRelArray<T>` seed.
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Default for RelArraySeed<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<'de, T> DeserializeSeed<'de> for RelArraySeed<T>
where
    Vec<T>: crate::de::Deserialize<'de>,
{
    type Value = Vec<T>;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        <Vec<T> as crate::de::Deserialize<'de>>::deserialize(RelArrayDeserializer(deserializer))
    }
}

/// A Deserializer wrapper that redirects only the outermost `deserialize_array` to `deserialize_rel_array`.
///
/// The elements are deserialized by the inner deserializer, so nested `hkArray<T>` are not affected.
struct RelArrayDeserializer<D>(D);

macro_rules! forward {
    ($($method:ident),+ $(,)?) => {
        $(
            #[inline]
            fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: Visitor<'de>,
            {
                self.0.$method(visitor)
            }
        )+
    };
}

impl<'de, D> Deserializer<'de> for RelArrayDeserializer<D>
where
    D: Deserializer<'de>,
{
    type Error = D::Error;

    #[inline]
    fn deserialize_array<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_rel_array(visitor)
    }

    forward!(
        deserialize_key,
        deserialize_class_index,
        deserialize_void,
        deserialize_bool,
        deserialize_char,
        deserialize_int8,
        deserialize_uint8,
        deserialize_int16,
        deserialize_uint16,
        deserialize_int32,
        deserialize_uint32,
        deserialize_int64,
        deserialize_uint64,
        deserialize_real,
        deserialize_vector4,
        deserialize_quaternion,
        deserialize_matrix3,
        deserialize_rotation,
        deserialize_qstransform,
        deserialize_matrix4,
        deserialize_transform,
        deserialize_pointer,
        deserialize_fixed_array,
        deserialize_rel_array,
        deserialize_class_index_seq,
        deserialize_variant,
        deserialize_cstring,
        deserialize_ulong,
        deserialize_half,
        deserialize_stringptr,
    );

    #[inline]
    fn deserialize_identifier<V>(
        self,
        size: ReadEnumSize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_identifier(size, visitor)
    }

    #[inline]
    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_enum(name, variants, visitor)
    }

    #[inline]
    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_struct(name, fields, visitor)
    }

    #[inline]
    fn deserialize_flags<V>(self, size: ReadEnumSize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_flags(size, visitor)
    }
}
