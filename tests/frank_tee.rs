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
fn tee<T: Clone, U>(
    tx: impl Effectful<Effect = Coproduct![Send<T>], Output = ()>,
    rx1: impl Effectful<Effect = Coproduct![Receive<T>], Output = U>,
    rx2: impl Effectful<Effect = Coproduct![Receive<T>], Output = U>,
) -> U {
    pin_mut!(tx);
    pin_mut!(rx1);
    pin_mut!(rx2);

    loop {
        #[eff]
        match poll_with_task_context(rx1.as_mut()) {
            v => return v,
            (Receive(_), recv1) =>
            {
                #[eff]
                match poll_with_task_context(rx2.as_mut()) {
                    v => return v,
                    (Receive(_), recv2) =>
                    {
                        #[eff]
                        match poll_with_task_context(tx.as_mut()) {
                            () => perform!(Abort),
                            (Send(msg), send) => {
                                send.waker().wake(());
                                recv1.waker().wake(msg.clone());
                                recv2.waker().wake(msg);
                            }
                        }
                    }
                }
            }
        }

        // Ideally, we want concurrent polling like this:
        // eff-match (tx.as_mut(), rx1.as_mut(), rx2.as_mut()) {
        //     (
        //         <Send(msg), send)>,
        //         <Receive(_), recv1>,
        //         <Receive(_), recv2>,
        //     ) => {
        //         send.waker().wake(());
        //         recv1.waker().wake(msg.clone());
        //         recv2.waker().wake(msg);
        //     }
        //     (_, v, _) | (_, _, v) => return v,
        //     ((), <_>, <_>) => perform!(Abort),
        // }
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
