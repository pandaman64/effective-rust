#![feature(fnbox, nll, generators, generator_trait)]
#![feature(get_type_id, core_intrinsics)]

#[macro_use]
mod eff;
use crate::eff::*;

#[derive(Debug)]
struct Foo(u32);

impl Effect for Foo {
    type Output = u32;
}

#[derive(Debug)]
struct Bar;

impl Effect for Bar {
    type Output = u32;
}

#[derive(Debug)]
enum Effects {
    F(Foo),
    B(Bar),
}

impl From<Foo> for Effects {
    fn from(f: Foo) -> Self {
        Effects::F(f)
    }
}

impl From<Bar> for Effects {
    fn from(b: Bar) -> Self {
        Effects::B(b)
    }
}

fn expr_with_effect(channel: Channel) -> crate::eff::ExprWithEffect<Effects, u32> {
    Box::new(move || {
        let v1: u32 = perform!(Foo(100), channel);
        let v2 = perform!(Bar, channel, u32);
        v1 * v2
    })
}

fn main() {
    let result = handle(expr_with_effect, |eff, k| match eff {
        Effects::F(f) => {
            println!("foo");
            k.run(f.0 * 2)
        }
        Effects::B(_b) => {
            println!("bar");
            k.run(4_u32)
        }
    });

    println!("{}", result);
}
