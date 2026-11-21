mod enum_or_flags;
mod structure;

use crate::{
    ClassMap,
    cpp_info::{Class, Member},
    get_class_map::get_inherited_class,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Result;

pub fn from_cpp_class(class: &Class, class_map: &ClassMap) -> Result<syn::File> {
    let class_code = {
        let struct_define = structure::generate(class)?;
        let impl_ser_for_struct = structure::impl_serialize(class, class_map);
        let impl_de_for_struct = structure::impl_deserialize(class, class_map)?;

        let enum_defines = enum_or_flags::generate(class);
        let impl_ser_for_enum_or_flags = enum_or_flags::impl_serialize(class);
        let impl_de_for_enum_or_flags = enum_or_flags::impl_deserialize(class);
        let enum_imports = ambiguous_enum_imports(class, class_map);

        quote::quote! {
            use super::class_requires::*;
            use super::*;
            #(#enum_imports)*

            #struct_define
            #impl_ser_for_struct
            #impl_de_for_struct

            #(#enum_defines)*
            #(#impl_ser_for_enum_or_flags)*
            #(#impl_de_for_enum_or_flags)*
        }
    };

    syn::parse2(class_code)
}

/// Explicitly import the enum/flags that are defined in other classes and whose names are ambiguous.
///
/// # Why need this?
/// All modules are glob imported by `use super::*;`. So if the same enum name is defined in multiple classes,
/// a member referring to the enum of the parent class becomes ambiguous. (explicit `use` takes precedence over glob)
///
/// e.g. hk2014(Fallout4):
/// `hclBoneSpaceMeshMeshDeformPNOperator.scaleNormalBehaviour` -> `hclBoneSpaceMeshMeshDeformOperator_::ScaleNormalBehaviour`
/// (`hclObjectSpaceMeshMeshDeformOperator` also has `ScaleNormalBehaviour`)
fn ambiguous_enum_imports(class: &Class, class_map: &ClassMap) -> Vec<TokenStream> {
    let mut imports: Vec<(String, String)> = Vec::new();
    let inherited = get_inherited_class(&class.name, class_map);

    // NOTE: Not only own members, the members of parents are also used in this module.(e.g. XML deserializer)
    let members = inherited.iter().flat_map(|c| c.members.iter());
    for member in members {
        let Some(enum_ref) = member.enum_ref.as_deref() else {
            continue;
        };
        if class.enums.iter().any(|e| e.name == enum_ref) {
            continue; // defined in this class.
        }
        let definers: Vec<&Class> = class_map
            .values()
            .filter(|c| c.enums.iter().any(|e| e.name == enum_ref))
            .collect();
        if definers.len() <= 1 {
            continue; // not ambiguous
        }

        // 1. Prefer the nearest parent class.
        // 2. Otherwise(e.g. the dump lacks the enum definition), guess it from other classes.
        let definer = inherited
            .iter()
            .rev()
            .find(|c| c.enums.iter().any(|e| e.name == enum_ref)).map_or_else(|| {
                let definer = guess_enum_definer(&class.name, member, &definers);
                tracing::warn!(
                    "{}({}): ambiguous enum `{enum_ref}` is not defined in parents. guessed: `{definer}`",
                    class.name,
                    member.name,
                );
                definer
            }, |c| c.name.as_ref());

        let import = (definer.to_string(), enum_ref.to_string());
        if !imports.contains(&import) {
            imports.push(import);
        }
    }

    imports
        .into_iter()
        .map(|(definer, enum_name)| {
            let module = format_ident!("{definer}_");
            let enum_name = format_ident!("{enum_name}");
            quote! { use super::#module::#enum_name; }
        })
        .collect()
}

/// Guess the definer of the enum that is not defined in the parents.
///
/// 1. The storage type(`vtype`, `vsubtype`) must be the same. Otherwise, the binary size is broken.
/// 2. Prefer the class whose name has the longest common prefix with this class name.
///    - e.g. `hknpConstraintCinfo.flags`(`FlagsEnum`: u8) -> `hknpConstraint::FlagsEnum`(u8)
///      (Not `hknpBodyQuality::FlagsEnum`(u32))
fn guess_enum_definer<'a>(class_name: &str, member: &Member, definers: &[&'a Class]) -> &'a str {
    let same_storage: Vec<&Class> = definers
        .iter()
        .copied()
        .filter(|c| {
            c.enums.iter().any(|e| {
                Some(e.name.as_ref()) == member.enum_ref.as_deref()
                    && e.vtype == member.vtype
                    && e.vsubtype == member.vsubtype
            })
        })
        .collect();
    let candidates = if same_storage.is_empty() {
        definers
    } else {
        same_storage.as_slice()
    };

    let common_prefix_len = |name: &str| {
        name.chars()
            .zip(class_name.chars())
            .take_while(|(a, b)| a == b)
            .count()
    };
    candidates
        .iter()
        .max_by_key(|c| common_prefix_len(&c.name))
        .map_or_else(|| definers[0].name.as_ref(), |c| c.name.as_ref())
}
