extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::{quote, ToTokens};
use syn::parse::Parse;

type TokenStream2 = proc_macro2::TokenStream;

enum CommaOrColon {
    Comma(syn::token::Comma),
    Colon(syn::token::Colon),
}

impl Parse for CommaOrColon {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        if input.peek(syn::token::Comma) {
            input.parse().map(CommaOrColon::Comma)
        } else {
            input.parse().map(CommaOrColon::Colon)
        }
    }
}

impl ToTokens for CommaOrColon {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            CommaOrColon::Comma(comma) => comma.to_tokens(tokens),
            CommaOrColon::Colon(colon) => colon.to_tokens(tokens),
        }
    }
}

fn wrap_pattern(tokens: impl ToTokens, n: usize) -> TokenStream2 {
    if n == 0 {
        tokens.into_token_stream()
    } else {
        wrap_pattern(quote! { eff::coproduct::Either::B(#tokens) }, n - 1)
    }
}

/// Declare the function to be an effectful computation whose effect type is the coproduct of the arguments
///
/// This macro transforms the function like what `async fn` does
#[proc_macro_attribute]
pub fn eff(attr: TokenStream, item: TokenStream) -> TokenStream {
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;

    if let Ok(syn::Item::Fn(mut func)) = syn::parse(item.clone()) {
        let effects_parser = Punctuated::<syn::Type, CommaOrColon>::parse_terminated;
        let types = effects_parser
            .parse(attr)
            .expect("failed to parse attribute");

        let mut ret = TokenStream2::new();

        let effects_type_name = quote! {
            eff::Coproduct![#types]
        };

        func.sig.output = syn::parse2(match func.sig.output {
            syn::ReturnType::Default => quote! {
                -> impl eff::Effectful<Output = (), Effect = #effects_type_name>
            },
            syn::ReturnType::Type(arrow, ty) => quote! {
                #arrow impl eff::Effectful<Output = #ty, Effect = #effects_type_name>
            },
        })
        .expect("return type is invalid");

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
        .expect("function block is invalid");

        // supress warning
        ret.extend(quote! {
            #[allow(unreachable_code)]
            #func
        });

        ret.into()
    } else if let Ok(syn::Expr::Match(mut m)) = syn::parse(item) {
        // Provide a nice pattern-match syntax for polling result of an effectful computation.
        // Concretely, convert a match clause of the form
        // ```
        // match <poll_expr> {
        //     <value_pattern> => <value_body>,
        //     (<eff1>, <k1>) => <eff_body1>,
        //     ...
        //     (<effN>, <kN>) => <eff_bodyN>,
        // }
        // ```
        // into
        // ```
        // match <poll_expr> {
        //     Complete(<value_pattern>) => <value_body>,
        //     Effect((A(<eff1>, <k1>) => <eff_body1>,
        //     ...
        //     Effect(B(B(...B(A(<effN>, <kN>))...))) => <eff_bodyN>,
        //     Effect(B(B(...B(B(__rest))...))) => reperform_rest!(__rest),
        // }
        // ```
        // TODO: Currently, this macro doesn't support other forms such as if guards.
        assert!(
            m.arms.len() >= 1,
            "An effect match clause must have an arm for the value pattern"
        );
        {
            let pat = m.arms[0].pat.clone();
            m.arms[0].pat = syn::parse2(quote! { eff::Event::Complete(#pat) })
                .expect("value pattern is invalid");
        }
        for (idx, ref mut arm) in m.arms[1..].iter_mut().enumerate() {
            let pat = arm.pat.clone();
            let wrapped = wrap_pattern(quote! { eff::coproduct::Either::A #pat }, idx);
            arm.pat = syn::parse2(quote! { eff::Event::Effect(#wrapped) })
                .expect(&format!("{}'th pattern is invalid", idx));
        }
        {
            let ident = quote! { __rest };
            let wrapped = wrap_pattern(&ident, m.arms.len() - 1);
            // allow unreachable as there can be no remaining effects
            m.arms.push(
                syn::parse2(quote! {
                    #[allow(unreachable_code)] eff::Event::Effect(#wrapped) => eff::reperform_rest!(#ident),
                })
                .expect("reperform arm is invalid"),
            );
        }
        m.into_token_stream().into()
    } else {
        panic!("eff couldn't parse the content");
    }
}

#[proc_macro_hack]
pub fn poll(input: TokenStream) -> TokenStream {
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;

    let parser = Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated;
    let exprs = parser.parse(input).expect("failed to parse input");

    let mut ret = TokenStream2::new();
    let names = exprs
        .iter()
        .enumerate()
        .map(|(idx, expr)| {
            let name = quote::format_ident!("__comp{}", idx);
            ret.extend(quote! {
                let mut #name = eff::Effectful::next_event(#expr);
            });
            name
        })
        .collect::<Vec<_>>();

    ret.extend(quote! {
        loop {
            let mut __all_occured = true;

            #(
                if let eff::Poll::Pending = eff::poll_with_task_context(unsafe { eff::pin_reexport::Pin::new_unchecked(&mut #names) }) {
                    __all_occured = false;
                }
            )*

            if __all_occured {
                break (#( unsafe { eff::pin_reexport::Pin::new_unchecked(&mut #names) }.take_event().unwrap() ),*);
            }
        }
    });

    quote!({ #ret }).into()
}
