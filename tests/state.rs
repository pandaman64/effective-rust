#![feature(generators, generator_trait, never_type)]

use rich_phantoms::PhantomCovariantAlwaysSendSync;

#[derive(Debug, Default)]
struct GetState<T>(PhantomCovariantAlwaysSendSync<T>);

impl<T> eff::Effect for GetState<T> {
    type Output = T;
}

#[derive(Debug)]
struct SetState<T>(T);

impl<T> eff::Effect for SetState<T> {
    type Output = ();
}

#[eff::eff(GetState<usize>, SetState<usize>)]
fn plus_one() -> String {
    let state: usize = eff::perform!(GetState::default());
    let ret = state.to_string();
    eff::perform!(SetState(state + 1));
    ret
}

#[eff::eff(GetState<usize>, SetState<usize>)]
fn double() -> String {
    let state: usize = eff::perform!(GetState::default());
    let ret = state.to_string();
    eff::perform!(SetState(state * 2));
    ret
}

#[eff::eff(GetState<usize>, SetState<usize>)]
fn do_something() -> String {
    let first = eff::perform_from!(plus_one());
    let second = eff::perform_from!(double());
    let third = eff::perform_from!(plus_one());
    format!("{}\n{}\n{}", first, second, third)
}

#[test]
fn test_state() {
    use eff::{Effectful, Pure};
    use std::cell::Cell;

    let state = Cell::new(10);
    let state_ref = &state;
    assert_eq!(
        do_something()
            .handle(
                |x| eff::pure(x).embed(),
                |e| {
                    match e.uninject() {
                        Ok((GetState(_), k)) => Ok(eff::Either::A(static move || {
                            eff::perform!(k.continuation(state_ref.get()))
                        })),
                        Err(e) => match e.uninject() {
                            Ok((SetState(x), k)) => Ok(eff::Either::B(static move || {
                                eff::perform!(k.continuation(state_ref.set(x)))
                            })),
                            Err(_) => unreachable!(),
                        },
                    }
                }
            )
            .run(),
        "10\n11\n22"
    );
    assert_eq!(state.into_inner(), 23);
}
