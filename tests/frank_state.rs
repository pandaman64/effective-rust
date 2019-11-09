#![feature(generators, never_type, stmt_expr_attributes, proc_macro_hygiene)]

use eff::*;
use pin_utils::pin_mut;
use std::marker::PhantomData;

#[derive(Debug)]
struct Get<T>(PhantomData<T>);

impl<T> Default for Get<T> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T> Effect for Get<T> {
    type Output = T;
}

#[derive(Debug)]
struct Set<T>(T);

impl<T> Effect for Set<T> {
    type Output = ();
}

#[eff]
fn run_state<S: Clone, T>(
    mut init: S,
    comp: impl Effectful<Output = T, Effect = Coproduct![Get<S>, Set<S>]>,
) -> (S, T) {
    pin_mut!(comp);

    loop {
        #[eff]
        match poll!(comp.as_mut()) {
            x => return (init, x),
            (Get(_), k) => k.waker().wake(init.clone()),
            (Set(v), k) => {
                init = v;
                k.waker().wake(());
            }
        }
    }
}

#[eff(Get<usize>, Set<usize>)]
fn plus_one() -> String {
    let state: usize = perform!(Get::default());
    let ret = state.to_string();
    perform!(Set(state + 1));
    ret
}

#[eff(Get<usize>, Set<usize>)]
fn double() -> String {
    let state: usize = perform!(Get::default());
    let ret = state.to_string();
    perform!(Set(state * 2));
    ret
}

#[eff(Get<usize>, Set<usize>)]
fn do_something() -> String {
    let first = perform_from!(plus_one());
    let second = perform_from!(double());
    let third = perform_from!(plus_one());
    format!("{}\n{}\n{}", first, second, third)
}

#[test]
fn test_frank_state() {
    assert_eq!(
        run_state(10, do_something()).block_on(),
        (23, "10\n11\n22".to_string())
    );
}
