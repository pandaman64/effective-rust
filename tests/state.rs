#![feature(generators, generator_trait, try_from, never_type)]

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
    let first = eff::invoke!(plus_one());
    let second = eff::invoke!(double());
    let third = eff::invoke!(plus_one());
    format!("{}\n{}\n{}", first, second, third)
}

#[test]
fn test_state() {
    use eff::Effectful;
    use std::cell::Cell;

    let state = Cell::new(10);
    assert_eq!(
        do_something()
            .handle(|GetState(_)| eff::HandlerResult::Resume(state.get()))
            .handle(|SetState(x)| eff::HandlerResult::Resume(state.set(x)))
            .run(|x| x)
            .unwrap(),
        "10\n11\n22"
    );
    assert_eq!(state.into_inner(), 23);
}
