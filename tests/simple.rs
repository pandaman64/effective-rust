#![feature(generators, generator_trait, never_type)]

use eff::*;

#[derive(Debug)]
struct Eff;
impl Effect for Eff {
    type Output = String;
}

#[test]
fn test_simple() {
    #[eff(Eff)]
    fn f() -> String {
        perform!(Eff)
    }

    let e = f();
    assert_eq!(
        e.handle(
            |x| eff::pure(x).embed(),
            |e| {
                e.on(|Eff, k| {
                    effectful! {
                        k.wake("Hello".into());
                        perform!(k.continuation())

                        // or
                        // perform!(cx.resume("Hello".into()))
                    }
                })
            }
        )
        .block_on(),
        "Hello"
    );
}
