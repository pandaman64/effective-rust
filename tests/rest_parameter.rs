#![feature(generators, never_type, stmt_expr_attributes, proc_macro_hygiene)]

use eff::*;
use pin_utils::pin_mut;
use std::sync::Arc;

#[derive(Debug)]
struct In;

impl Effect for In {
    type Output = ();
}

#[derive(Debug)]
struct Out;

impl Effect for Out {
    type Output = ();
}

#[eff(Out : R)]
fn func<R>(comp: impl Effectful<Output = (), Effect = Coproduct![In: R]>) {
    pin_mut!(comp);

    loop {
        #[eff]
        match poll_with_task_context(comp.as_mut()) {
            () => {
                perform!(Out);
                perform!(Out);
                break;
            }
            (In, k) => {
                println!("got In");
                k.waker().wake(());
            }
        }
    }
}

#[test]
fn test_rest_parameter() {
    use coproduct::Either::*;
    use Event::*;
    let incomp = effectful! {
        perform!(In);
    };

    struct DoNothingNotify;

    impl Notify for DoNothingNotify {
        fn wake(&self) {}
    }

    let context = Context::from_notify(Arc::new(DoNothingNotify));

    let outcomp = func(incomp);
    pin_mut!(outcomp);

    let mut count = 0;

    loop {
        match outcomp.as_mut().poll(&context) {
            Poll::Event(Complete(())) => break,
            Poll::Event(Effect(A(Out, k))) => {
                count += 1;
                k.waker().wake(());
            }
            Poll::Event(Effect(B(never))) => {
                let _: ! = never;
            }
            Poll::Pending => unreachable!(),
        }
    }

    assert_eq!(count, 2);
}
