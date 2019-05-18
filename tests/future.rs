#![feature(generators, never_type, async_await)]

use eff::*;
use futures::executor::block_on;

#[test]
fn test_future() {
    let e = lazy(|| println!("running in a future!"));
    block_on(e.into_future());
}

#[runtime::test]
async fn test_runtime() {
    struct Choose;

    impl Effect for Choose {
        type Output = bool;
    }

    let result = (effectful! {
        if perform!(Choose) {
            42
        } else {
            24
        }
    })
    .effect::<Coproduct![Choose]>()
    .handle(handler! {
        x => x,
        Choose, k => {
            perform!(k.resume(true)) + 100
        }
    })
    .into_future()
    .await;

    assert_eq!(result, 142);
}
