#![feature(fnbox, nll, generators, generator_trait, never_type)]

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
struct Baz;
impl Effect for Baz {
    type Output = !;
}

fn main() {
    let expr_with_effect = eff! {
        let index = perform!(Foo(2));
        let s = perform!(Bar);
        perform!(Baz);
        s.chars().nth(index).unwrap()
    };

    let result = handle(
        expr_with_effect,
        handler! {
            Foo(Foo(idx), k) => {
                println!("foo");
                k.run::<Foo>(idx * 2)
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
