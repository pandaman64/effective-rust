#![feature(generators, generator_trait, try_from)]

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

    assert_eq!(run(
        f(),
        |x| x,
        handler! {
            Eff => {
                resume!("Hello".into())
            }
        },
    ), "Hello");
}
