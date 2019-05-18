#![feature(generators, never_type)]

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
        e.handle(handler! {
            x => x,
            Eff, k => {
                k.waker().wake("Hello".into());
                perform!(k.continuation())
            }
        })
        .block_on(),
        "Hello"
    );
}
