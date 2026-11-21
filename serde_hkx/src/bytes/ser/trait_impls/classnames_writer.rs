//! Trait that defines a set of dedicated methods for writing Havok Class data.
use super::Align as _;
use byteorder::{ByteOrder, WriteBytesExt as _};
use havok_serde::HavokClass;
use indexmap::IndexMap;
use std::{
    collections::{HashMap, HashSet},
    io::{self, Cursor, Write as _},
};

/// This must be retained because the information about which C++ class was written to which location
/// will be in `virtual_fixup.src`.
pub type ClassStartsMap = IndexMap<&'static str, u32>;

/// Trait that defines a set of dedicated methods for writing Havok Class data.
/// # Note
///This is implemented in the type that serializes the classes.
pub trait ClassNamesWriter {
    /// Write `__classnames__` section contents.
    ///
    /// - `file_version`: [`HkxHeader::file_version`](crate::bytes::serde::hkx_header::HkxHeader::file_version).
    ///   It is used to select the signatures of meta classes(`hkClass`, etc.).
    fn write_classnames_section<O>(
        &self,
        writer: &mut Cursor<Vec<u8>>,
        file_version: i32,
    ) -> io::Result<ClassStartsMap>
    where
        O: ByteOrder;
}

macro_rules! impl_writer {
    ($({ $($generics:tt)* } $ty:ty => $iter:ident),+ $(,)?) => {
        $(
            impl<$($generics)*> ClassNamesWriter for $ty
            where
                V: HavokClass,
            {
                #[inline]
                fn write_classnames_section<O>(
                    &self,
                    writer: &mut Cursor<Vec<u8>>,
                    file_version: i32,
                ) -> io::Result<ClassStartsMap>
                where
                    O: ByteOrder,
                {
                    write_classnames::<O, V>(self.$iter(), writer, file_version)
                }
            }
        )+
    };
}

impl_writer!(
    { K, V } IndexMap<K, V> => values,
    { K, V } HashMap<K, V> => values,
    { V } Vec<V> => iter,
    { V } HashSet<V> => iter,
    { V } &[V] => iter,
);

/// Signatures of meta classes(`hkClass`, `hkClassMember`, `hkClassEnum`, `hkClassEnumItem`).
///
/// - `file_version` <= 8(hk2010: SkyrimLE/SE)
/// - `file_version` >= 11(hk2014: Fallout4)
const fn meta_class_signatures(file_version: i32) -> [u32; 4] {
    if file_version >= 11 {
        [0x33D42383, 0xB0EFA719, 0x8A3609CF, 0xCE6F8A6C]
    } else {
        [0x75585EF6, 0x5C7EA4C2, 0x8A3609CF, 0xCE6F8A6C]
    }
}

/// Write `__classnames__` section contents.
fn write_classnames<'a, O, V>(
    classes: impl Iterator<Item = &'a V>,
    writer: &mut Cursor<Vec<u8>>,
    file_version: i32,
) -> io::Result<ClassStartsMap>
where
    O: ByteOrder,
    V: HavokClass + 'a,
{
    let classnames_section_start = writer.position() as u32;

    // These classes are meta-information, such as C++ class signatures, and are always considered
    // to exist because they are already defined in the SDK.
    // - If a class is used in the __data__ section, an `hkClass` is used to represent that information
    // - Similarly, `hkClassEnum` is used depending on whether or not enum is used,
    //   but here we assume that it is always used and write it in advance.
    const META_CLASS_NAMES: [&[u8]; 4] = [
        b"\x09hkClass\0",
        b"\x09hkClassMember\0",
        b"\x09hkClassEnum\0",
        b"\x09hkClassEnumItem\0",
    ];
    for (signature, name) in meta_class_signatures(file_version)
        .into_iter()
        .zip(META_CLASS_NAMES)
    {
        writer.write_u32::<O>(signature)?;
        writer.write_all(name)?;
    }

    let mut class_map = ClassStartsMap::new(); // To write `virtual_fixup`
    for class in classes {
        let class_name = class.name();
        if class_map.contains_key(class_name) {
            continue;
        }

        writer.write_u32::<O>(class.signature().into())?;
        writer.write_u8(0x09)?;

        let name_start = writer.position() as u32;
        writer.write_all(class_name.as_bytes())?;
        writer.write_u8(0)?;

        if classnames_section_start > name_start {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Must be  name_start position >= __classnames__ abs position",
            ));
        }

        let name_offset = name_start - classnames_section_start;
        class_map.insert(class_name, name_offset);
    }
    writer.align(16, 0xff)?;

    Ok(class_map)
}
