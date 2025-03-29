use super::to_rust_token::{member_to_rust_type, to_rust_field_ident};
use crate::cpp_info::FlagValues;
use crate::get_class_map::serde_borrow_attr;
use crate::{
    bail_syn_err,
    cpp_info::{Member, TypeKind},
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::Error;

/// C++ member info -> Rust field token
pub(super) fn gen_field(member: &Member, class_name: &str) -> Result<TokenStream, Error> {
    let Member {
        name,
        vtype,
        arrsize,
        has_ref,
        flags,
        ..
    } = member;

    let field_type = member_to_rust_type(member, class_name)?;

    let serde_borrow_attr = serde_borrow_attr(*has_ref);

    let serde_default = if flags.contains(FlagValues::SERIALIZE_IGNORED) {
        // XML skips fields with the SERIALIZE_IGNORED flag, so the only way is to put the default value
        quote! { #[cfg_attr(feature = "serde", serde(default))] }
    } else {
        quote! {}
    };

    // `Default` implementations with huge sizes such as [0u8; 256] are not automatically supported, so use `educe` crate to define them.
    let arr_custom_attr = if *arrsize > 32 {
        // NOTE: This can only be solved with `#[serde(with=“”)` because we cannot put a feature inside a feature
        // see https://github.com/jonasbb/serde_with/issues/355
        let as_value = format!("::serde_with::As::<[::serde_with::Same; {arrsize}]>"); // NOTE: need `serde_with`
        let serde_with_attr = quote! {
            #[cfg_attr(feature = "json_schema", schemars(schema_with = "make_large_int_array_schema"))]
            #[cfg_attr(feature = "serde", serde(with = #as_value))]
        };

        let default_attr = match vtype {
            TypeKind::Int8
            | TypeKind::Uint8
            | TypeKind::Int32
            | TypeKind::Uint32
            | TypeKind::Int16
            | TypeKind::Uint16
            | TypeKind::Int64
            | TypeKind::Uint64 => {
                quote! {
                    #[educe(Default(expression = core::array::from_fn(|_idx| Default::default())))]
                }
            }
            _ => {
                bail_syn_err!(
                    "Giant fixed-size arrays are supported only for Int or Uint 8~64. But got {vtype}"
                )
            }
        };
        quote! {
            #serde_with_attr
            #default_attr
        }
    } else {
        quote! {}
    };

    let doc = field_doc_tokens(member);
    let field_name = to_rust_field_ident(name);
    Ok(quote! {
        #doc
        #arr_custom_attr
        #serde_borrow_attr
        #serde_default
        #[cfg_attr(feature = "json_schema", schemars(rename = #name))]
        #[cfg_attr(feature = "serde", serde(rename = #name))]
        pub #field_name: #field_type
    })
}

fn field_doc_tokens(member: &Member) -> TokenStream {
    let Member {
        name,
        ctype,
        offset_x86,
        offset_x86_64,
        type_size_x86,
        type_size_x86_64,
        flags,
        ..
    } = member;

    let name = format!(" - name: `{name}`(ctype: `{ctype}`)");
    let offsets = format!(" - offset: `{offset_x86:3}`(x86)/`{offset_x86_64:3}`(x86_64)");
    let type_sizes =
        format!(" - type_size: `{type_size_x86:3}`(x86)/`{type_size_x86_64:3}`(x86_64)");
    let flags_doc = if flags.bits() == 0 {
        quote! {} // NOTE: If `FlagsNone`, the flag does not exist and is not displayed.
    } else {
        let doc = format!(" - flags: `{flags}`");
        quote! { #[doc = #doc]}
    };

    quote! {
        /// # C++ Info
        #[doc = #name]
        #[doc = #offsets]
        #[doc = #type_sizes]
        #flags_doc
    }
}
