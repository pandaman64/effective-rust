extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn eff(attr: TokenStream, item: TokenStream) -> TokenStream {
    use syn::parse::Parser;

    // TODO: parse #[eff(parameter = T)]
    let effects_parser = syn::punctuated::Punctuated::<syn::Type, Token![,]>::parse_terminated;
    let types = effects_parser
        .parse(attr)
        .expect("failed to parse attribute");

    for t in types.iter() {
        println!("type: {:?}", t);
    }

    let names: Vec<_> = types
        .iter()
        .enumerate()
        .map(|(i, _)| syn::Ident::new(&format!("variant_{}", i), proc_macro2::Span::call_site()))
        .collect();

    let item: syn::Item = syn::parse(item).expect("failed to parse item");
    println!("item: {:?}", item);

    if let syn::Item::Fn(mut func) = item {
        let mut ret = proc_macro2::TokenStream::new();

        let effects_type_name = syn::Ident::new(
            &format!("__effects_{}", func.ident),
            proc_macro2::Span::call_site(),
        );
        let channel_type_name = syn::Ident::new(
            &format!("__channel_{}", func.ident),
            proc_macro2::Span::call_site(),
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
        println!("eff = {}", effects_decl);
        ret.extend(effects_decl);

        // build channel type
        let channel_decl = quote! {
            #[derive(Debug)]
            enum #channel_type_name {
                #(#names ( <#types as eff::Effect>::Output ) ),*
            }
        };
        println!("channel = {}", channel_decl);
        ret.extend(channel_decl);

        // implement necessary traits for each effect
        for (name, ty) in names.iter().zip(types.iter()) {
            let impl_from = quote! {
                impl std::convert::From<#ty> for #effects_type_name {
                    fn from(v: #ty) -> Self {
                        #effects_type_name :: #name (v)
                    }
                }
            };
            println!("impl from = {}", impl_from);
            ret.extend(impl_from);

            let impl_try_from = quote! {
                impl std::convert::TryFrom<#effects_type_name> for #ty {
                    type Error = #effects_type_name;
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
            println!("impl try_from = {}", impl_try_from);
            ret.extend(impl_try_from);

            let impl_channel = quote! {
                impl eff::Channel<#ty> for #channel_type_name {
                    fn from(v: <#ty as eff::Effect>::Output) -> Self {
                        #channel_type_name :: #name (v)
                    }

                    fn into(self) -> <#ty as eff::Effect>::Output {
                        match self {
                            #channel_type_name :: #name (v) => v,
                            _ => unreachable!(),
                        }
                    }
                }
            };
            println!("impl channel = {}", impl_channel);
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
                    Box<std::ops::Generator<Yield = eff::Suspension<E, C, R>, Return = ()>>,
                >
            },
            syn::ReturnType::Type(arrow, ty) => quote! {
                #arrow eff::WithEffectInner<
                    #effects_type_name,
                    #channel_type_name,
                    Box<std::ops::Generator<Yield = eff::Suspension<E, C, R>, Return = #ty>>,
                >
            },
        })
        .unwrap();

        let original_block = func.block;
        func.block = syn::parse2(quote! {
            {
                eff::WithEffectInner::<#effects_type_name, #channel_type_name, _>::new(
                    Box::new(#[allow(unreachable_code)] static move || {
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
        println!("all = {}", ret);

        ret.into()
    } else {
        panic!("item must be a function");
    }
}

struct Handler {
    pattern: syn::Pat,
    _arrow: Token![=>],
    expr: syn::Expr,
}

impl syn::parse::Parse for Handler {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Handler {
            pattern: input.parse()?,
            _arrow: input.parse()?,
            expr: input.parse()?,
        })
    }
}

struct ResumeMuncher<'a>(&'a syn::Path);

impl<'a> syn::visit_mut::VisitMut for ResumeMuncher<'a> {
    fn visit_expr_macro_mut(&mut self, i: &mut syn::ExprMacro) {
        if let Some(last) = i.mac.path.segments.last() {
            let value = last.value();
            if value.arguments.is_empty()
                && value.ident == syn::Ident::new("new_resume", proc_macro2::Span::call_site())
            {
                let path = self.0;
                let mut tts = quote! { @#path , };
                tts.extend(std::mem::replace(
                    &mut i.mac.tts,
                    proc_macro2::TokenStream::new(),
                ));
                i.mac.tts = tts;
                syn::visit_mut::visit_expr_macro_mut(self, i)
            }
        }
    }
}

#[proc_macro_hack::proc_macro_hack]
pub fn nonexhaustive_handler(input: TokenStream) -> TokenStream {
    use syn::parse::Parser;

    let parser = syn::punctuated::Punctuated::<Handler, Token![,]>::parse_terminated;
    let handlers = parser.parse(input).expect("parsing a handler failed");

    let mut eff_types = vec![];
    let mut inner = proc_macro2::TokenStream::new();
    for Handler {
        pattern, mut expr, ..
    } in handlers.into_iter()
    {
        let effect_type_path = match &pattern {
            syn::Pat::Struct(p) => p.path.clone(),
            syn::Pat::TupleStruct(p) => p.path.clone(),
            pattern @ syn::Pat::Ident(_) | pattern @ syn::Pat::Path(_) => {
                unimplemented!("not supported yet {:?}", pattern)
            }
            pattern => panic!("effect type must be known: {:?}", pattern),
        };

        eff_types.push(effect_type_path.clone());

        // munching
        syn::visit_mut::visit_expr_mut(&mut ResumeMuncher(&effect_type_path), &mut expr);

        let code = quote! {
            match eff.try_into() {
                Ok(pattern) => return eff::HandlerResult::Exit(#expr),
                Err(e) => eff = e,
            }
        };

        inner.extend(code);
    }

    let eff_types = &eff_types;

    (quote! {{
        use std::convert::TryInto;
        fn __effect_handler<E, R, C>(mut eff: E) -> eff::HandlerResult<R, C>
        where
            E: #(TryInto<#eff_types , Error = E>)+*,
            C: #(eff::Channel<#eff_types>)+*,
        {
            {
                #inner
            }
            eff::HandlerResult::Unhandled
        }
        __effect_handler
    }})
    .into()
}
