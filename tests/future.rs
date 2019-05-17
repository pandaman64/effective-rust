#![feature(generators, never_type)]

use eff::*;
use futures::executor::block_on;

#[test]
fn test_future() {
    let e = lazy(|| println!("running in a future!"));
    block_on(e.into_future());
}
