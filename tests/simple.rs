#![feature(generators, never_type)]
#![feature(trace_macros)]

extern crate eff;
use eff::*;

struct Eff;
impl Effect for Eff {
    type Output = String;
}

#[test]
fn test_simple() {
    handle(
        eff! {
            perform!(Eff)
        },
        |x| x,
        handler! {
            A @ Eff[_, k] => {
                resume!(k, "Hello".into())
            }
        },
    );
}
