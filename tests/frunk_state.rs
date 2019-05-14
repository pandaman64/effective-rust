#![feature(generators, never_type)]

use context::poll_with_task_context;
use coproduct::Either::{A, B};
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
        match poll_with_task_context(comp.as_mut()) {
            Poll::Done(x) => return (init, x),
            Poll::Effect(A(Get(_), cx)) => cx.waker().wake(init.clone()),
            Poll::Effect(B(A(Set(v), cx))) => {
                init = v;
                cx.waker().wake(());
            }
            Poll::Effect(B(B(_))) => unreachable!(),
            Poll::Pending => yield Suspension::Pending,
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
fn test_frunk_state() {
    assert_eq!(
        run_state(10, do_something()).block_on(),
        (23, "10\n11\n22".to_string())
    );
}
