#![feature(generators, never_type)]

use eff::*;

#[derive(Debug)]
struct Eff;

impl Effect for Eff {
    type Output = i32;
}

#[test]
fn test_conv() {
    let e = (effectful! {
        perform!(Eff) + perform!(Eff)
    })
    .effect::<Coproduct![Eff]>();

    assert_eq!(
        e.handle(handler! {
            x => (x as i32).to_string(),
            Eff, k => perform!(k.resume(10)),
        })
        .block_on(),
        "20"
    );
}
