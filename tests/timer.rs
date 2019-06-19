#![feature(async_await, generators, never_type)]

use eff::*;
use futures::compat::Future01CompatExt;
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
            sleep(Duration::from_millis(100))
                .compat()
                .await
                .expect("timer runtime is not correctly set up"); // panic here
            eprintln!("bar");
        });

        let mut pool = ThreadPool::new().expect("failed to create thread pool");
        pool.run(fut);
    });

    let err = handle.join().unwrap_err();
    let msg: &String = err.downcast_ref().unwrap();
    assert!(msg.starts_with("timer runtime is not correctly set up"));
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

        effectful! {
            pin_mut!(comp);

            loop {
                match await_poll!(comp.as_mut()) {
                    AwaitedPoll::Done(v) => return v,
                    AwaitedPoll::Effect(A(Delay(instant), k)) => {
                        // assume timer is already initialized
                        let delay = handle.lock().unwrap().as_ref().unwrap().delay(instant);
                        perform_from!(future::future::from_future(delay.compat())).unwrap();
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
