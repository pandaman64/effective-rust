#![feature(generators, generator_trait, never_type)]

use eff::*;

#[derive(Debug)]
struct Eff;
impl Effect for Eff {
    type Output = String;
}

/*
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
            |e| { e.on(|Eff, k| static move || perform!(k.continuation("Hello".into()))) }
        )
        .run(),
        "Hello"
    );
}*/

#[test]
fn test_raw() {
    let effectful = from_generator(static move || {
        yield <Coproduct![Eff]>::inject(Eff);
        get_key(|key| key.get::<String>())
    });
    let result = effectful.handle(
        |x| pure(x).embed(),
        |_e| {
            Ok(from_generator(static move || {
                get_key(|key| {
                    key.set::<String>("Hello".into());
                });
                yield <Coproduct![Continue<String>]>::inject(Continue::new());
                let result = get_key(|key| key.get::<String>());
                format!("{} World!", result)
            }))
        },
    );
    assert_eq!(result.block_on(), "Hello World!");
}
