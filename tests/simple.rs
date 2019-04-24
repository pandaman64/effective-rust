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
            |_e| {
                Ok(from_generator(static move || {
                    get_key(|key| {
                        key.set::<String>("Hello".into());
                    });
                    yield Suspension::Effect(<Coproduct![Continue<String>]>::inject(
                        Continue::new(),
                    ));
                    get_key(|key| key.get::<String>())
                }))
            }
        )
        .block_on(),
        "Hello"
    );
}
