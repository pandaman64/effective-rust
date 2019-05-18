#![feature(generators, never_type)]

use eff::coproduct::Either::A;
use eff::*;
use pin_utils::pin_mut;
use std::marker::PhantomData;
use std::sync::Arc;

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
    use AwaitedPoll::*;

    pin_mut!(tx);
    pin_mut!(rx);

    loop {
        let send = await_poll!(tx.as_mut());
        let recv = await_poll!(rx.as_mut());

        match (send, recv) {
            (Effect(A(Send(msg), send)), Effect(A(Receive(_), recv))) => {
                send.waker().wake(());
                recv.waker().wake(msg);
            }
            (Effect(_), Effect(_)) => unreachable!(), // Rust can't prove this arm is unreachable
            (_, Done(v)) => return v,
            (Done(()), Effect(_)) => perform!(Abort),
        }
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

    struct DoNothingNotify;

    impl Notify for DoNothingNotify {
        fn wake(&self) {}
    }

    let context = Context::from_notify(Arc::new(DoNothingNotify));

    match pipe.poll(&context) {
        Poll::Done(10) => {}
        x => panic!("invalid output: {:?}", x),
    }
}
