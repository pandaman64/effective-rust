#![feature(generators, never_type, stmt_expr_attributes, proc_macro_hygiene)]

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
    pin_mut!(tx);
    pin_mut!(rx);

    loop {
        // Not that good compared to Frank's simultaneous pattern-match
        let (txe, rxe) = poll!(tx.as_mut(), rx.as_mut());
        #[eff]
        match txe {
            () =>
            {
                #[eff]
                match rxe {
                    v => return v,
                    (_, _) => perform!(Abort),
                }
            }
            (Send(msg), send) =>
            {
                #[eff]
                match rxe {
                    v => return v,
                    (Receive(_), recv) => {
                        send.waker().wake(());
                        recv.waker().wake(msg);
                    }
                }
            }
        }

        // Ideally, this should be written like the following, where <...> matches against an effect:
        // eff-match (tx.as_mut(), rx.as_mut()) {
        //     (<Send(msg), send>, <Receive(_), recv>) => {
        //         send.waker().wake(());
        //         recv.waker().wake(msg);
        //     }
        //     (_, v) => return v,
        //     ((), <_>) => perform!(Abort),
        // }
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
        Poll::Event(Event::Complete(10)) => {}
        x => panic!("invalid output: {:?}", x),
    }
}
