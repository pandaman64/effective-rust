extern crate proc_macro;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

type TokenStream2 = proc_macro2::TokenStream;

#[proc_macro_attribute]
pub fn eff(attr: TokenStream, item: TokenStream) -> TokenStream {
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;

    // TODO: parse #[eff(lifetime = 'a)]
    let effects_parser = Punctuated::<syn::Type, Token![,]>::parse_terminated;
    let types = effects_parser
        .parse(attr)
        .expect("failed to parse attribute");
    let types = &types;

    let item: syn::Item = syn::parse(item).expect("failed to parse item");

    if let syn::Item::Fn(mut func) = item {
        let mut ret = TokenStream2::new();

        let effects_type_name = quote! {
            eff::Coproduct![#types]
        };

        func.decl.output = syn::parse2(match func.decl.output {
            syn::ReturnType::Default => quote! {
                -> impl std::ops::Generator<Yield = #effects_type_name, Return = ()>
            },
            syn::ReturnType::Type(arrow, ty) => quote! {
                #arrow impl std::ops::Generator<Yield = #effects_type_name, Return = #ty>
            },
        })
        .unwrap();

        let original_block = func.block;
        func.block = syn::parse2(quote! {
            {
                static move || {
                    if false {
                        yield unreachable!();
                    }

                    #original_block
                }
            }
        })
        .unwrap();

        // supress warning
        ret.extend(quote! {
            #[allow(unreachable_code)]
            #func
        });

        ret.into()
    } else {
        panic!("item must be a function");
    }
}
