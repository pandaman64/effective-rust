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

    let e = f();
    pin_utils::pin_mut!(e);

    assert_eq!(
        run(
            e,
            |x| x,
            handler! {
                Eff => {
                    resume!("Hello".into())
                }
            },
        ),
        "Hello"
    );
}
