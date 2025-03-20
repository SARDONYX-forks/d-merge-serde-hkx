// SPDX-License-Identifier: Apache-2.0 OR MIT
//
// # Forked serde (ver. 1.0.202)
// And serde holds the same license as Rust. https://github.com/rust-lang/rust/pull/43498
// See: https://github.com/serde-rs/serde/commit/58b3af4c2915c3ae789778a11f3b7a468c1cec17
//
// The default implementation does not fully express Havok's special XML format.
//
// # Modification details
// - Rust std types -> Havok Types
// - Changed serde method to Havok XML& binary data signatures, which are easier to modify.
//
// # Memory optimization of `visit_array`
// MaybeUninit leaks memory if an error occurs during initialization, but we learned from the
// following how to use Guard to prevent leaks if necessary.
//
// SPDX-FileCopyrightText: 2017-2020 The array-init developers
// https://github.com/Manishearth/array-init/blob/master/src/lib.rs
//! Implement `Deserialize`

use super::{Deserialize, Deserializer, Error, Visitor};
use crate::de::Unexpected;
use crate::de::{seed::InPlaceSeed, size_hint, SeqAccess};
use crate::lib::*;
use havok_types::*;

////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_deserialize {
    ($ty:ty, $visitor:tt, $de_method:tt) => {
        // ty = U8<'de>
        impl<'de> Deserialize<'de> for $ty {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct InnerVisitor;
                impl<'a> Visitor<'a> for InnerVisitor {
                    type Value = $ty; // ty = U8<'a>

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str(stringify!($ty))
                    }

                    fn $visitor<E>(self, v: $ty) -> Result<Self::Value, E>
                    where
                        E: Error,
                    {
                        Ok(v)
                    }
                }

                Ok(tri!(deserializer.$de_method(InnerVisitor)))
            }
        }
    };
}

impl_deserialize!((), visit_void, deserialize_void);
impl_deserialize!(char, visit_char, deserialize_char);
impl_deserialize!(bool, visit_bool, deserialize_bool);

impl_deserialize!(f16, visit_half, deserialize_half);
impl_deserialize!(f32, visit_real, deserialize_real);

impl_deserialize!(Vector4, visit_vector4, deserialize_vector4);
impl_deserialize!(Quaternion, visit_quaternion, deserialize_quaternion);
impl_deserialize!(Matrix3, visit_matrix3, deserialize_matrix3);
impl_deserialize!(Rotation, visit_rotation, deserialize_rotation);
impl_deserialize!(QsTransform, visit_qstransform, deserialize_qstransform);
impl_deserialize!(Matrix4, visit_matrix4, deserialize_matrix4);
impl_deserialize!(Transform, visit_transform, deserialize_transform);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_deserialize_with_lifetime {
    ($ty1:ty, $ty2:ty, $visitor:tt, $de_method:tt) => {
        impl<'de: 'a, 'a> Deserialize<'de> for $ty1 {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct InnerVisitor;
                impl<'a> Visitor<'a> for InnerVisitor {
                    type Value = $ty2;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str(stringify!($ty))
                    }

                    fn $visitor<E>(self, v: $ty2) -> Result<Self::Value, E>
                    where
                        E: Error,
                    {
                        Ok(v)
                    }
                }

                Ok(tri!(deserializer.$de_method(InnerVisitor)))
            }
        }
    };
}

impl_deserialize_with_lifetime!(I8<'de>, I8<'a>, visit_int8, deserialize_int8);
impl_deserialize_with_lifetime!(I16<'de>, I16<'a>, visit_int16, deserialize_int16);
impl_deserialize_with_lifetime!(I32<'de>, I32<'a>, visit_int32, deserialize_int32);
impl_deserialize_with_lifetime!(I64<'de>, I64<'a>, visit_int64, deserialize_int64);

impl_deserialize_with_lifetime!(U8<'de>, U8<'a>, visit_uint8, deserialize_uint8);
impl_deserialize_with_lifetime!(U16<'de>, U16<'a>, visit_uint16, deserialize_uint16);
impl_deserialize_with_lifetime!(U32<'de>, U32<'a>, visit_uint32, deserialize_uint32);
impl_deserialize_with_lifetime!(U64<'de>, U64<'a>, visit_uint64, deserialize_uint64);

impl_deserialize_with_lifetime!(
    StringPtr<'de>,
    StringPtr<'a>,
    visit_stringptr,
    deserialize_stringptr
);
impl_deserialize_with_lifetime!(
    CString<'de>,
    CString<'a>,
    visit_cstring,
    deserialize_cstring
);

impl_deserialize_with_lifetime!(
    Variant<'de>,
    Variant<'a>,
    visit_variant,
    deserialize_variant
);
impl_deserialize_with_lifetime!(
    Pointer<'de>,
    Pointer<'a>,
    visit_pointer,
    deserialize_pointer
);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'de> Deserialize<'de> for Ulong {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct InnerVisitor;
        impl<'a> Visitor<'a> for InnerVisitor {
            type Value = Ulong;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Ulong")
            }

            fn visit_uint32<E>(self, v: U32<'a>) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match v {
                    U32::Number(n) => Ok(Ulong::new(n as u64)),
                    unexpected => Err(Error::invalid_type(Unexpected::Uint32(unexpected), &self)),
                }
            }

            fn visit_uint64<E>(self, v: U64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match v {
                    U64::Number(n) => Ok(Ulong::new(n)),
                    unexpected => Err(Error::invalid_type(Unexpected::Uint64(unexpected), &self)),
                }
            }
        }

        Ok(tri!(deserializer.deserialize_ulong(InnerVisitor)))
    }
}

macro_rules! seq_vec_impl_with_lifetime {
    ($($ty:ty),+ $(,)? => $fn_name:tt,  $fn_name_seed:tt) => {
        $(
            #[cfg(any(feature = "std", feature = "alloc"))]
            #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
            impl<'de> Deserialize<'de> for Vec<$ty> {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: Deserializer<'de>,
                {
                    struct VecVisitor;

                    impl<'de> Visitor<'de> for VecVisitor {
                        type Value = Vec<$ty>;

                        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                            formatter.write_str("a sequence")
                        }

                        fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                        where
                            A: SeqAccess<'de>,
                        {
                            let capacity = size_hint::cautious::<$ty>(seq.size_hint());
                            let mut values = Vec::with_capacity(capacity);

                            while let Some(value) = tri!(seq.$fn_name()) {
                                values.push(value);
                            }

                            Ok(values)
                        }
                    }

                    let visitor = VecVisitor;
                    deserializer.deserialize_array(visitor)
                }

                fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
                where
                    D: Deserializer<'de>,
                {
                    struct VecInPlaceVisitor<'a, 'de: 'a>(&'a mut Vec<$ty>);

                    impl<'a, 'de: 'a> Visitor<'de> for VecInPlaceVisitor<'a, 'de> {
                        type Value = ();

                        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                            formatter.write_str("a sequence")
                        }

                        fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                        where
                            A: SeqAccess<'de>,
                        {
                            let hint = size_hint::cautious::<$ty>(seq.size_hint());
                            if let Some(additional) = hint.checked_sub(self.0.len()) {
                                self.0.reserve(additional);
                            }

                            for i in 0..self.0.len() {
                                let next = {
                                    let next_place = InPlaceSeed(&mut self.0[i]);
                                    tri!(seq.$fn_name_seed(next_place))
                                };
                                if next.is_none() {
                                    self.0.truncate(i);
                                    return Ok(());
                                }
                            }

                            while let Some(value) = tri!(seq.$fn_name()) {
                                self.0.push(value);
                            }

                            Ok(())
                        }
                    }

                    deserializer.deserialize_array(VecInPlaceVisitor(place))
                }
            }
        )*
    };
}

seq_vec_impl_with_lifetime!(Pointer<'de>, U8<'de>, U16<'de>, U32<'de>, U64<'de>, I8<'de>, I16<'de>, I32<'de>, I64<'de> => next_primitive_element, next_primitive_element_seed);
seq_vec_impl_with_lifetime!(StringPtr<'de> => next_stringptr_element, next_stringptr_element_seed);
seq_vec_impl_with_lifetime!(CString<'de> => next_cstring_element, next_cstring_element_seed);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! seq_vec_impl {
    ($($ty:ty),+ $(,)? => $fn_name:tt,  $fn_name_seed:tt) => {
      $(
        #[cfg(any(feature = "std", feature = "alloc"))]
        #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
        impl<'de> Deserialize<'de> for Vec<$ty> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct VecVisitor;

                impl<'de> Visitor<'de> for VecVisitor
                {
                    type Value = Vec<$ty>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a sequence")
                    }

                    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let capacity = size_hint::cautious::<$ty>(seq.size_hint());
                        let mut values = Vec::with_capacity(capacity);

                        while let Some(value) = tri!(seq.$fn_name()) {
                            values.push(value);
                        }

                        Ok(values)
                    }
                }

                let visitor = VecVisitor;
                deserializer.deserialize_array(visitor)
            }

            fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
            where
                D: Deserializer<'de>,
            {
                struct VecInPlaceVisitor<'a>(&'a mut Vec<$ty>);

                impl<'a, 'de: 'a> Visitor<'de> for VecInPlaceVisitor<'a>
                {
                    type Value = ();

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a sequence")
                    }

                    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let hint = size_hint::cautious::<$ty>(seq.size_hint());
                        if let Some(additional) = hint.checked_sub(self.0.len()) {
                            self.0.reserve(additional);
                        }

                        for i in 0..self.0.len() {
                            let next = {
                                let next_place = InPlaceSeed(&mut self.0[i]);
                                tri!(seq.$fn_name_seed(next_place))
                            };
                            if next.is_none() {
                                self.0.truncate(i);
                                return Ok(());
                            }
                        }

                        while let Some(value) = tri!(seq.$fn_name()) {
                            self.0.push(value);
                        }

                        Ok(())
                    }
                }

                deserializer.deserialize_array(VecInPlaceVisitor(place))
            }
        }
      )*
    };
}

seq_vec_impl!((), bool, char, f16, f32, Ulong => next_primitive_element, next_primitive_element_seed);
seq_vec_impl!(Vector4, Quaternion, Matrix3, Rotation, QsTransform, Matrix4, Transform => next_math_element, next_math_element_seed);

////////////////////////////////////////////////////////////////////////////////
// For `hkClass`, etc vector.

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, T> Deserialize<'de> for Vec<T>
where
    T: Deserialize<'de> + crate::HavokClass,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct VecVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for VecVisitor<T>
        where
            T: Deserialize<'de> + crate::HavokClass,
        {
            type Value = Vec<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a sequence")
            }

            fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let capacity = size_hint::cautious::<T>(seq.size_hint());
                let mut values = Vec::with_capacity(capacity);

                while let Some(value) = tri!(seq.next_class_element()) {
                    values.push(value);
                }

                Ok(values)
            }
        }

        let visitor = VecVisitor(PhantomData);
        deserializer.deserialize_array(visitor)
    }

    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Deserializer<'de>,
    {
        struct VecInPlaceVisitor<'a, T>(&'a mut Vec<T>);

        impl<'a, 'de: 'a, T> Visitor<'de> for VecInPlaceVisitor<'a, T>
        where
            T: Deserialize<'de> + crate::HavokClass,
        {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a sequence")
            }

            fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let hint = size_hint::cautious::<T>(seq.size_hint());
                if let Some(additional) = hint.checked_sub(self.0.len()) {
                    self.0.reserve(additional);
                }

                for i in 0..self.0.len() {
                    let next = {
                        let next_place = InPlaceSeed(&mut self.0[i]);
                        tri!(seq.next_class_element_seed(next_place))
                    };
                    if next.is_none() {
                        self.0.truncate(i);
                        return Ok(());
                    }
                }

                while let Some(value) = tri!(seq.next_class_element()) {
                    self.0.push(value);
                }

                Ok(())
            }
        }

        deserializer.deserialize_array(VecInPlaceVisitor(place))
    }
}

////////////////////////////////////////////////////////////////////////////////

struct ArrayVisitor<A> {
    marker: PhantomData<A>,
}
struct ArrayInPlaceVisitor<'a, A: 'a>(&'a mut A);

impl<A> ArrayVisitor<A> {
    fn new() -> Self {
        ArrayVisitor {
            marker: PhantomData,
        }
    }
}

macro_rules! array_impls {
    ($($ty:ty),+ $(,)? => $fn_name:tt,  $fn_name_seed:tt) => {
        $(
            impl<'de, const N: usize> Visitor<'de> for ArrayVisitor<[$ty; N]>
            {
                type Value = [$ty; N];

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str(&format!("an array of length {N}"))
                }

                fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: SeqAccess<'de>,
                {
                    let mut array = arrayvec::ArrayVec::<$ty, N>::new();
                    for _ in 0..N {
                        if let Some(value_i) = tri!(seq.$fn_name()) {
                            array.push(value_i);
                        } else {
                            break;
                        };
                    }
                    match array.take().into_inner() {
                        Ok(array) => Ok(array),
                        Err(array) => Err(Error::invalid_length(
                            array.len(),
                            &(N.to_string().as_str()),
                        )),
                    }
                }
            }

            impl<'a, 'de, const N: usize> Visitor<'de> for ArrayInPlaceVisitor<'a, [$ty; N]>
            where
            {
                type Value = ();

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str(&format!("an array of length {N}"))
                }

                #[inline]
                fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: SeqAccess<'de>,
                {
                    let mut fail_idx = None;
                    for (idx, dest) in self.0[..].iter_mut().enumerate() {
                        if tri!(seq.$fn_name_seed(InPlaceSeed(dest))).is_none() {
                            fail_idx = Some(idx);
                            break;
                        }
                    }
                    if let Some(idx) = fail_idx {
                        return Err(Error::invalid_length(idx, &self));
                    }
                    Ok(())
                }
            }

            impl<'de, const N: usize> Deserialize<'de> for [$ty; N]
            {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: Deserializer<'de>,
                {
                    deserializer.deserialize_fixed_array(ArrayVisitor::<[$ty; N]>::new())
                }

                fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
                where
                    D: Deserializer<'de>,
                {
                    deserializer.deserialize_fixed_array(ArrayInPlaceVisitor(place))
                }
            }
        )+
    }
}

array_impls!((), bool, char, U8<'de>, U16<'de>, U32<'de>, U64<'de>, I8<'de>, I16<'de>, I32<'de>, I64<'de>, f16, f32, Ulong => next_primitive_element, next_primitive_element_seed);
array_impls!(Vector4, Quaternion, Matrix3, Rotation, QsTransform, Matrix4, Transform => next_math_element, next_math_element_seed);

impl<'de, T, const N: usize> Visitor<'de> for ArrayVisitor<[T; N]>
where
    T: Deserialize<'de> + crate::HavokClass,
{
    type Value = [T; N];

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(&format!("an array of length {N}"))
    }

    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut array = arrayvec::ArrayVec::<T, N>::new();
        for _ in 0..N {
            if let Some(value_i) = tri!(seq.next_class_element()) {
                array.push(value_i);
            } else {
                break;
            };
        }
        match array.take().into_inner() {
            Ok(array) => Ok(array),
            Err(array) => Err(Error::invalid_length(
                array.len(),
                &(N.to_string().as_str()),
            )),
        }
    }
}

impl<'de, T, const N: usize> Visitor<'de> for ArrayInPlaceVisitor<'_, [T; N]>
where
    T: Deserialize<'de> + crate::HavokClass,
{
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(&format!("an array of length {N}"))
    }

    #[inline]
    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut fail_idx = None;
        for (idx, dest) in self.0[..].iter_mut().enumerate() {
            if tri!(seq.next_class_element_seed(InPlaceSeed(dest))).is_none() {
                fail_idx = Some(idx);
                break;
            }
        }
        if let Some(idx) = fail_idx {
            return Err(Error::invalid_length(idx, &self));
        }
        Ok(())
    }
}

impl<'de, T, const N: usize> Deserialize<'de> for [T; N]
where
    T: Deserialize<'de> + crate::HavokClass,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_fixed_array(ArrayVisitor::<[T; N]>::new())
    }

    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_fixed_array(ArrayInPlaceVisitor(place))
    }
}

////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_deserialize_for_map {
    ($($map_ident:ident),+ $(,)?) => {
        $(
        impl<'de, T> Deserialize<'de> for $map_ident<std::borrow::Cow<'de, str>, T>
        where
            T: Deserialize<'de> + crate::HavokClass,
        {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct MapVisitor<T>(PhantomData<T>);

                impl<'de, T> Visitor<'de> for MapVisitor<T>
                where
                    T: Deserialize<'de> + crate::HavokClass,
                {
                    type Value = $map_ident<Cow<'de, str>, T>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a sequence")
                    }

                    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let capacity = size_hint::cautious::<T>(seq.size_hint());
                        let mut values: Self::Value = $map_ident::with_capacity(capacity);

                        while let Some(value) = tri!(seq.next_class_element()) {
                            values.insert(tri!(seq.class_ptr()), value);
                        }
                        Ok(values)
                    }
                }

                let visitor = MapVisitor(PhantomData);
                deserializer.deserialize_class_index_seq(visitor)
            }
        }

        impl<'de, T> Deserialize<'de> for $map_ident<Pointer<'de>, T>
        where
            T: Deserialize<'de> + crate::HavokClass,
        {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct MapVisitor<T>(PhantomData<T>);

                impl<'de, T> Visitor<'de> for MapVisitor<T>
                where
                    T: Deserialize<'de> + crate::HavokClass,
                {
                    type Value = $map_ident<Pointer<'de>, T>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a sequence")
                    }

                    fn visit_array<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let capacity = size_hint::cautious::<T>(seq.size_hint());
                        let mut values: Self::Value = $map_ident::with_capacity(capacity);

                        while let Some(value) = tri!(seq.next_class_element()) {
                            values.insert(Pointer::new(tri!(seq.class_ptr())), value);
                        }
                        Ok(values)
                    }
                }

                let visitor = MapVisitor(PhantomData);
                deserializer.deserialize_class_index_seq(visitor)
            }
        }
        )*
    };
}

#[cfg(all(feature = "indexmap", feature = "std"))]
const _: () = {
    use indexmap::IndexMap;
    impl_deserialize_for_map!(IndexMap);
};

#[cfg(any(feature = "std", feature = "alloc"))]
const _: () = {
    use std::collections::HashMap;
    impl_deserialize_for_map!(HashMap);
};
