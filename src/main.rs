#![feature(fnbox, nll, generators, generator_trait)]
#![feature(get_type_id, core_intrinsics)]
#![feature(trace_macros)]

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

fn main() {
    let expr_with_effect = eff! {
        let index = perform!(Foo(2));
        let s = perform!(Bar);
        s.chars().nth(index).unwrap()
    };

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
