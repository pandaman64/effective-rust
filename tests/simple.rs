#![feature(generators)]

use eff::*;

struct Eff;
impl Effect for Eff {
    type Output = String;
}

#[test]
fn test_simple() {
    run(
        eff! {
            perform!(Eff)
        },
        |x| x,
        handler! {
            A @ Eff[_] => {
                resume!("Hello".into())
            }
        },
    );
}
