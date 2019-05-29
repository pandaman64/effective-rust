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
fn tee<T: Clone, U>(
    tx: impl Effectful<Effect = Coproduct![Send<T>], Output = ()>,
    rx1: impl Effectful<Effect = Coproduct![Receive<T>], Output = U>,
    rx2: impl Effectful<Effect = Coproduct![Receive<T>], Output = U>,
) -> U {
    use AwaitedPoll::*;

    pin_mut!(tx);
    pin_mut!(rx1);
    pin_mut!(rx2);

    loop {
        match await_join!(tx.as_mut(), rx1.as_mut(), rx2.as_mut()) {
            (
                Effect(A(Send(msg), send)),
                Effect(A(Receive(_), recv1)),
                Effect(A(Receive(_), recv2)),
            ) => {
                send.waker().wake(());
                recv1.waker().wake(msg.clone());
                recv2.waker().wake(msg);
            }
            (Effect(_), Effect(_), Effect(_)) => unreachable!(), // Rust can't prove this arm is unreachable
            (_, Done(v), _) | (_, _, Done(v)) => return v,
            (Done(()), Effect(_), Effect(_)) => perform!(Abort),
        }
    }
}

#[test]
fn test_pipe() {
    let tx = effectful! {
        for i in 1..10 {
            perform!(Send(i));
        }
    };

    let rx1 = effectful! {
        let mut sum = 0;
        for _ in 1..5 {
            sum += perform!(Receive(PhantomData));
        }
        sum
    };

    let rx2 = effectful! {
        let mut prod = 1;
        for _ in 1..4 {
            prod *= perform!(Receive(PhantomData));
        }
        prod
    };

    let tee = tee(tx, rx1, rx2);
    pin_mut!(tee);

    struct DoNothingNotify;

    impl Notify for DoNothingNotify {
        fn wake(&self) {}
    }

    let context = Context::from_notify(Arc::new(DoNothingNotify));

    match tee.poll(&context) {
        Poll::Done(6) => {}
        x => panic!("invalid output: {:?}", x),
    }
}
