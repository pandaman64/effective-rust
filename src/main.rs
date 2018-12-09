#![feature(fnbox, nll, generators, generator_trait)]
#![feature(get_type_id, core_intrinsics)]

#[macro_use]
mod eff;
use crate::eff::*;

#[derive(Debug)]
struct Foo(usize);

impl Effect for Foo {
    type Output = usize;
}

#[derive(Debug)]
struct Bar;

impl Effect for Bar {
    type Output = String;
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

fn expr_with_effect(channel: Channel) -> crate::eff::ExprWithEffect<Effects, char> {
    Box::new(move || {
        let index = perform!(Foo(2), channel);
        let s = perform!(Bar, channel);
        s.chars().nth(index).unwrap()
    })
}

fn main() {
    let result = handle(expr_with_effect, |eff, k| match eff {
        Effects::F(f) => {
            println!("foo");
            k.run::<Foo>(f.0 * 2)
        }
        Effects::B(_b) => {
            println!("bar");
            k.run::<Bar>("Hello, World!".into())
        }
    });

    println!("{}", result);
}
