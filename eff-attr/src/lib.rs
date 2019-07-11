extern crate proc_macro;

use proc_macro::TokenStream;

type TokenStream2 = proc_macro2::TokenStream;

/// Declare the function to be an effectful computation whose effect type is the coproduct of the arguments
///
/// This macro transforms the function like what `async fn` does
#[proc_macro_attribute]
pub fn eff(attr: TokenStream, item: TokenStream) -> TokenStream {
    use quote::quote;
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;

    let effects_parser = Punctuated::<syn::Type, syn::token::Comma>::parse_terminated;
    let types = effects_parser
        .parse(attr)
        .expect("failed to parse attribute");

    let item: syn::Item = syn::parse(item).expect("failed to parse item");

    if let syn::Item::Fn(mut func) = item {
        let mut ret = TokenStream2::new();

        let effects_type_name = quote! {
            eff::Coproduct![#types]
        };

        func.decl.output = syn::parse2(match func.decl.output {
            syn::ReturnType::Default => quote! {
                -> impl eff::Effectful<Output = (), Effect = #effects_type_name>
            },
            syn::ReturnType::Type(arrow, ty) => quote! {
                #arrow impl eff::Effectful<Output = #ty, Effect = #effects_type_name>
            },
        })
        .unwrap();

        let original_block = func.block;
        func.block = syn::parse2(quote! {
            {
                eff::from_generator(static move || {
                    if false {
                        yield unreachable!();
                    }

                    #original_block
                })
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
