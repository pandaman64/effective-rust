#![feature(generators, generator_trait, try_from, never_type)]

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
        e.handle(|Eff| HandlerResult::Resume("Hello".into()))
            .run(|x| x),
        "Hello"
    );
}
