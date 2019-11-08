#![feature(generators, never_type)]

use eff::*;

struct Print<'a>(&'a str);

impl Effect for Print<'_> {
    type Output = ();
}

fn main() {
    (effectful! {
        perform!(Print("A"));
        perform!(Print("B"))
    })
    .effect::<Coproduct![Print]>()
    .handle(handler! {
        x => x,
        Print(msg), k => {
            k.waker().wake(());
            perform!(k.continuation());
            println!("{}", msg)
        }
    })
    .block_on();
}
