#![cfg(feature = "futures-compat")]
#![feature(generators, never_type, stmt_expr_attributes, proc_macro_hygiene)]

use eff::*;
use futures::executor::LocalPool;
use pin_utils::pin_mut;
use tokio_timer::{delay_for, timer::Timer};

use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

#[test]
fn test_fail_timer_future() {
    let handle = thread::spawn(|| {
        let fut = Box::pin(async {
            eprintln!("foo");
            delay_for(Duration::from_millis(100)).await;
            eprintln!("bar");
        });

        let mut pool = LocalPool::new();
        pool.run_until(fut);
    });

    assert!(handle.join().is_err());
}

#[test]
fn test_timer_effect() {
    #[derive(Debug)]
    struct Delay(Instant);

    impl Effect for Delay {
        type Output = ();
    }

    #[eff(Delay)]
    fn delay_for(duration: Duration) {
        perform!(Delay(Instant::now() + duration))
    }

    fn run_tokio_timer<T>(
        comp: impl Effectful<Output = T, Effect = Coproduct![Delay]>,
    ) -> impl Effectful<Output = T, Effect = !> {
        let handle = Arc::new(Mutex::new(None));
        let current = thread::current();

        // run a timer thread
        thread::spawn({
            let handle = handle.clone();
            move || {
                let mut timer = Timer::default();
                {
                    let mut lock = handle.lock().unwrap();
                    *lock = Some(timer.handle());
                }

                current.unpark();

                loop {
                    timer.turn(None).unwrap();
                }
            }
        });

        // wait for the timer thread to set up
        thread::park();
        let handle = handle.lock().unwrap().clone().unwrap();

        effectful! {
            pin_mut!(comp);

            loop {
                #[eff]
                match poll!(comp.as_mut()) {
                    v => return v,
                    (Delay(instant), k) => {
                        let delay = handle.delay(instant);
                        perform_from!(futures_compat::future::from_future(delay));
                        k.waker().wake(());
                    },
                }
            }
        }
    }

    let comp = run_tokio_timer(effectful! {
        eprintln!("foo");
        perform_from!(delay_for(Duration::from_millis(100)));
        eprintln!("bar");
    });

    comp.block_on();
}
