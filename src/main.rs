#![feature(fnbox, nll, generators, generator_trait)]
#![feature(get_type_id, core_intrinsics)]

#[macro_use]
mod eff;
use crate::eff::*;

#[derive(Debug)]
enum Effects {
    Foo,
    Bar,
}

fn expr_with_effect(channel: Channel) -> crate::eff::ExprWithEffect<Effects, u32> {
    Box::new(move || {
        let v1: u32 = perform!(Effects::Foo, channel);
        let v2 = perform!(Effects::Bar, channel, u32);
        v1 * v2
    })
}

fn main() {
    let result = handle(expr_with_effect, |eff, k| match eff {
        Effects::Foo => {
            println!("foo");
            k.run(13_u32)
        }
        Effects::Bar => {
            println!("bar");
            k.run(4_u32)
        }
    });

    println!("{}", result);
}
