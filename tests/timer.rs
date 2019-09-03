#![feature(generators, never_type)]

use eff::*;
use futures::executor::ThreadPool;
use pin_utils::pin_mut;
use tokio_timer::{sleep, timer::Timer};

use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

#[test]
fn test_fail_timer_future() {
    let handle = thread::spawn(|| {
        let fut = Box::pin(async {
            eprintln!("foo");
            sleep(Duration::from_millis(100)).await;
            eprintln!("bar");
        });

        let mut pool = ThreadPool::new().expect("failed to create thread pool");
        pool.run(fut);
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
    fn sleep(duration: Duration) {
        perform!(Delay(Instant::now() + duration))
    }

    fn run_tokio_timer<T>(
        comp: impl Effectful<Output = T, Effect = Coproduct![Delay]>,
    ) -> impl Effectful<Output = T, Effect = !> {
        use coproduct::Either::*;

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
                match await_poll!(comp.as_mut()) {
                    AwaitedPoll::Done(v) => return v,
                    AwaitedPoll::Effect(A(Delay(instant), k)) => {
                        let delay = handle.delay(instant);
                        perform_from!(future::future::from_future(delay));
                        k.waker().wake(());
                    },
                    AwaitedPoll::Effect(B(_)) => unreachable!(),
                }
            }
        }
    }

    let comp = run_tokio_timer(effectful! {
        eprintln!("foo");
        perform_from!(sleep(Duration::from_millis(100)));
        eprintln!("bar");
    });

    comp.block_on();
}
