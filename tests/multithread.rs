#![feature(generators, generator_trait, never_type)]

use std::thread;
use std::time::Duration;

#[derive(Debug)]
struct LongComputation;

impl eff::Effect for LongComputation {
    type Output = u32;
}

#[test]
fn test_multithread() {
    #[eff::eff(LongComputation)]
    fn computation() -> u32 {
        eff::perform!(LongComputation) + 100
    }

    use eff::Effectful;

    assert_eq!(
        computation()
            .handle(
                |x| eff::pure(x).embed(),
                |e| e.on(|LongComputation, k| {
                    eff::effectful! {
                        thread::spawn({
                            let waker = k.waker();
                            move || {
                                thread::sleep(Duration::from_millis(500));
                                waker.wake(42)
                            }
                        });
                        eff::perform!(k.continuation()) + 200
                    }
                })
            )
            .block_on(),
        342,
    );
}
