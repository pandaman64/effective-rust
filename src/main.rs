#![feature(fnbox, nll, generators, generator_trait, never_type)]

#[macro_use]
mod eff;
use crate::eff::*;

#[derive(Debug)]
enum Foo {
    This(usize),
    That(usize),
}
impl Effect for Foo {
    type Output = usize;
}

#[derive(Debug)]
struct Bar;
impl Effect for Bar {
    type Output = String;
}

#[derive(Debug)]
struct Baz;
impl Effect for Baz {
    type Output = !;
}

fn main() {
    let expr_with_effect = eff! {
        let i1 = perform!(Foo::This(1));
        let i2 = perform!(Foo::That(3));
        let s = perform!(Bar);
        if i1 + i2 >= s.len() {
            perform!(Baz);
        }
        s.chars().nth(i1 + i2).unwrap()
    };

    let result = handle(
        expr_with_effect,
        handler! {
            Foo(foo, k) => {
                println!("foo");
                match foo {
                    Foo::This(idx) => k.run::<Foo>(idx * 2),
                    Foo::That(idx) => k.run::<Foo>(idx - 1),
                }
            },
            Bar(_eff, k) => {
                println!("bar");
                k.run::<Bar>("Hello, World!".into())
            },
            Baz(_eff, _k) => {
                println!("baz");
                'x'
            }
        },
    );

    println!("{}", result);
}
