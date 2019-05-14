#![feature(generators, never_type)]

use eff::context::get_task_context;
use eff::*;
use pin_utils::pin_mut;
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Eq)]
struct Send<T>(T);

impl<T> Effect for Send<T> {
    type Output = ();
}

#[derive(Debug, PartialEq, Eq)]
struct Receive<T>(PhantomData<T>);

impl<T> Effect for Receive<T> {
    type Output = T;
}

#[derive(Debug, PartialEq, Eq)]
struct Abort;

impl Effect for Abort {
    type Output = !;
}

#[eff(Abort)]
fn pipe<T, U>(
    tx: impl Effectful<Effect = Coproduct![Send<T>], Output = ()>,
    rx: impl Effectful<Effect = Coproduct![Receive<T>], Output = U>,
) -> U {
    use handled::HandlerArgument::*;

    pin_mut!(tx);
    pin_mut!(rx);

    let mut s = await_poll!(&mut tx);
    let mut r = await_poll!(&mut rx);

    loop {
        let (send, receive) = match (s, r) {
            (
                Effect(coproduct::Either::A(Send(v), _)),
                Effect(coproduct::Either::A(Receive(_), _)),
            ) => ((), v),
            (Effect(_), Effect(_)) => unreachable!(),
            (_, Done(v)) => return v,
            (Done(()), _) => perform!(Abort),
        };

        get_task_context().set(send);
        s = await_poll!(&mut tx);
        get_task_context().set(receive);
        r = await_poll!(&mut rx);
    }
}

#[test]
fn test_pipe() {
    let tx = effectful! {
        for i in 0..10 {
            perform!(Send(i));
        }
    };

    let rx = effectful! {
        let mut sum = 0;
        for _ in 0..5 {
            sum += perform!(Receive(PhantomData));
        }
        sum
    };

    let pipe = pipe(tx, rx);
    pin_mut!(pipe);

    let context = Context::current();

    match pipe.poll(&context) {
        Poll::Done(10) => {}
        x => panic!("invalid output: {:?}", x),
    }
}
