#![feature(generators, generator_trait, never_type, impl_trait_in_bindings)]

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
                e.on(|Eff, key| {
                    effectful! {
                        key.wake("Hello".into());
                        perform!(key.continuation())

                        // or
                        // perform!(key.resume("Hello".into()))
                    }
                })
            }
        )
        .block_on(),
        "Hello"
    );
}
