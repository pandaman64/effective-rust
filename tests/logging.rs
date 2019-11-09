#![feature(generators, never_type, stmt_expr_attributes, proc_macro_hygiene)]

use eff::*;
use pin_utils::pin_mut;
use std::fmt::Debug;

#[eff(E : R)]
fn logged<E, R, C>(comp: C) -> C::Output
where
    C: Effectful<Effect = Coproduct![E: R]>,
    E: Effect + Debug,
    E::Output: Debug,
{
    pin_mut!(comp);

    loop {
        #[eff]
        match poll!(comp.as_mut()) {
            v => return v,
            (e, k) => {
                // structured logging such as `tracing` could be used
                eprintln!("an effect happend: {:?}", e);
                let result = perform!(e);
                eprintln!("an effect handled: {:?}", result);
                k.waker().wake(result);
            }
        }
    }
}

#[derive(Debug)]
struct LoggedEffect;

impl Effect for LoggedEffect {
    type Output = &'static str;
}

#[derive(Debug)]
struct NonLoggedEffect;

impl Effect for NonLoggedEffect {
    type Output = &'static str;
}

#[test]
fn test_logging() {
    let comp = effectful! {
        perform!(LoggedEffect);
        perform!(NonLoggedEffect);
    };

    logged(comp)
        .effect::<Coproduct![LoggedEffect, NonLoggedEffect]>()
        .handle(handler! {
            v => v,
            LoggedEffect, k => eff::perform!(k.resume("hello logged")),
        })
        .handle(handler! {
            v => v,
            NonLoggedEffect, k => eff::perform!(k.resume("hello nonlogged")),
        })
        .block_on();
}
