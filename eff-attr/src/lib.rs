extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

type TokenStream2 = proc_macro2::TokenStream;

#[proc_macro_attribute]
pub fn eff(attr: TokenStream, item: TokenStream) -> TokenStream {
    use proc_macro2::Span;
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;
    use syn::Ident;

    // TODO: parse #[eff(parameter = T)]
    let effects_parser = Punctuated::<syn::Type, Token![,]>::parse_terminated;
    let types = effects_parser
        .parse(attr)
        .expect("failed to parse attribute");

    let names: Vec<_> = types
        .iter()
        .enumerate()
        .map(|(i, _)| Ident::new(&format!("variant_{}", i), Span::call_site()))
        .collect();

    let item: syn::Item = syn::parse(item).expect("failed to parse item");

    if let syn::Item::Fn(mut func) = item {
        let mut ret = TokenStream2::new();

        let effects_type_name = Ident::new(
            &format!("__effects_{}", func.ident),
            Span::call_site(),
        );
        let channel_type_name = Ident::new(
            &format!("__channel_{}", func.ident),
            Span::call_site(),
        );
        let types = &types;
        let names = &names;

        // build effect type
        let effects_decl = quote! {
            #[derive(Debug)]
            enum #effects_type_name {
                #(#names ( #types ) ),*
            }
        };
        ret.extend(effects_decl);

        // build channel type
        let channel_decl = quote! {
            #[derive(Debug)]
            enum #channel_type_name {
                #(#names ( <#types as eff::Effect>::Output ) ),*
            }
        };
        ret.extend(channel_decl);

        // implement necessary traits for each effect
        for (name, ty) in names.iter().zip(types.iter()) {
            let impl_from = quote! {
                impl std::convert::From<#ty> for #effects_type_name {
                    #[inline]
                    fn from(v: #ty) -> Self {
                        #effects_type_name :: #name (v)
                    }
                }
            };
            ret.extend(impl_from);

            let impl_try_from = quote! {
                impl std::convert::TryFrom<#effects_type_name> for #ty {
                    type Error = #effects_type_name;
                    #[inline]
                    fn try_from(v: #effects_type_name) -> std::result::Result<Self, #effects_type_name>
                    {
                        if let #effects_type_name :: #name (v) = v {
                            Ok(v)
                        } else {
                            Err(v)
                        }
                    }
                }
            };
            ret.extend(impl_try_from);

            let impl_channel = quote! {
                impl eff::Channel<#ty> for #channel_type_name {
                    #[inline]
                    fn from(v: <#ty as eff::Effect>::Output) -> Self {
                        #channel_type_name :: #name (v)
                    }

                    #[inline]
                    fn into(self) -> <#ty as eff::Effect>::Output {
                        match self {
                            #channel_type_name :: #name (v) => v,
                            _ => unreachable!(),
                        }
                    }
                }
            };
            ret.extend(impl_channel);
        }

        func.decl.generics.params.push(
            syn::parse2(quote! {
                E: #(std::convert::From<#types>)+*
            })
            .unwrap(),
        );

        func.decl.generics.params.push(
            syn::parse2(quote! {
                C: #(eff::Channel<#types>)+*
            })
            .unwrap(),
        );

        func.decl.generics.params.push(
            syn::parse2(quote! {
                R
            })
            .unwrap(),
        );

        func.decl.output = syn::parse2(match func.decl.output {
            syn::ReturnType::Default => quote! {
                -> eff::WithEffectInner<
                    #effects_type_name,
                    #channel_type_name,
                    std::pin::Pin<std::boxed::Box<std::ops::Generator<Yield = eff::Suspension<E, C, R>, Return = ()>>>,
                >
            },
            syn::ReturnType::Type(arrow, ty) => quote! {
                #arrow eff::WithEffectInner<
                    #effects_type_name,
                    #channel_type_name,
                    std::pin::Pin<std::boxed::Box<std::ops::Generator<Yield = eff::Suspension<E, C, R>, Return = #ty>>>,
                >
            },
        })
        .unwrap();

        let original_block = func.block;
        func.block = syn::parse2(quote! {
            {
                eff::WithEffectInner::<#effects_type_name, #channel_type_name, _>::new(
                    std::boxed::Box::pin(#[allow(unreachable_code)] static move || {
                        if false {
                            yield unreachable!();
                        }

                        #original_block
                    })
                )
            }
        })
        .unwrap();

        ret.extend(quote! { #func });

        ret.into()
    } else {
        panic!("item must be a function");
    }
}

enum HandlerEntry {
    ReturnType {
        _arrow: Token![->],
        return_type: syn::Type,
    },
    Handler {
        pattern: syn::Pat,
        _arrow: Token![=>],
        expr: syn::Expr,
    },
}

impl syn::parse::Parse for HandlerEntry {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![->]) {
            Ok(HandlerEntry::ReturnType {
                _arrow: input.parse()?,
                return_type: input.parse()?,
            })
        } else {
            Ok(HandlerEntry::Handler {
                pattern: input.parse()?,
                _arrow: input.parse()?,
                expr: input.parse()?,
            })
        }
    }
}

struct ResumeMuncher<'a>(&'a TokenStream2);

impl<'a> syn::visit_mut::VisitMut for ResumeMuncher<'a> {
    fn visit_expr_macro_mut(&mut self, i: &mut syn::ExprMacro) {
        if let Some(last) = i.mac.path.segments.last() {
            let value = last.value();
            if value.arguments.is_empty()
                && value.ident == syn::Ident::new("resume", proc_macro2::Span::call_site())
            {
                let path = self.0;
                let mut tts = quote! { @#path , };
                tts.extend(std::mem::replace(
                    &mut i.mac.tts,
                    TokenStream2::new(),
                ));
                i.mac.tts = tts;
                syn::visit_mut::visit_expr_macro_mut(self, i)
            }
        }
    }
}

#[proc_macro_hack::proc_macro_hack]
pub fn handler(input: TokenStream) -> TokenStream {
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;

    let parser = Punctuated::<HandlerEntry, Token![,]>::parse_terminated;
    let entries = parser.parse(input).expect("parsing a handler failed");

    let mut effect_types = vec![];
    let mut inner = TokenStream2::new();
    let mut return_type = None;

    for entry in entries.into_iter()
    {
        match entry {
            HandlerEntry::ReturnType {
                return_type: ty, ..
            } => {
                assert!(return_type.is_none(), "return type have to appear only once");
                return_type = Some(ty);
            }
            HandlerEntry::Handler {
                pattern, mut expr, ..
            } => {
                use syn::Pat::*;
                use quote::ToTokens;

                let effect_type = match &pattern {
                    Struct(p) => {
                        p.path.clone().into_token_stream()
                    },
                    TupleStruct(p) => {
                        p.path.clone().into_token_stream()
                    },
                    Ident(p) if p.by_ref == None && p.mutability == None && p.subpat == None => {
                        p.ident.clone().into_token_stream()
                    },
                    Path(p) => {
                        p.clone().into_token_stream()
                    },
                    pattern => panic!("effect type must be known: {:?}", pattern),
                };

                effect_types.push(effect_type.clone());

                // munching
                syn::visit_mut::visit_expr_mut(&mut ResumeMuncher(&effect_type), &mut expr);

                let code = quote! {
                    match TryInto::<#effect_type>::try_into(eff) {
                        Ok(#pattern) => return eff::HandlerResult::Exit(#expr),
                        Err(e) => eff = e,
                    }
                };

                inner.extend(code);
            }
        }
    }

    let effect_types = &effect_types;

    match return_type {
        Some(ty) => quote! {{
            use std::convert::TryInto;
            #[allow(unreachable_code)]
            #[inline]
            fn __effect_handler<E, C>(mut eff: E) -> eff::HandlerResult<#ty, C>
            where
                E: #(TryInto<#effect_types , Error = E>)+*,
                C: #(eff::Channel<#effect_types>)+*,
            {
                {
                    #inner
                }
                eff::HandlerResult::Unhandled
            }
            __effect_handler
        }},
        // generic handler
        None => quote! {{
            use std::convert::TryInto;
            #[allow(unreachable_code)]
            #[inline]
            fn __effect_handler<E, R, C>(mut eff: E) -> eff::HandlerResult<R, C>
            where
                E: #(TryInto<#effect_types , Error = E>)+*,
                C: #(eff::Channel<#effect_types>)+*,
            {
                {
                    #inner
                }
                eff::HandlerResult::Unhandled
            }
            __effect_handler
        }},
    }
    .into()
}
