#![feature(generators, never_type)]

extern crate eff;
use eff::*;

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

mod effects {
    #[derive(Debug)]
    pub struct Baz;
    impl super::Effect for Baz {
        type Output = !;
    }
}

#[test]
fn test_example() {
    let expr_with_effect = eff! {
        let i1 = perform!(Foo::This(1));
        let i2 = perform!(Foo::That(3));
        let s = perform!(Bar);
        if i1 + i2 >= s.len() {
            perform!(effects::Baz);
        }
        s.chars().nth(i1 + i2).unwrap()
    };

    let result = handle(
        expr_with_effect,
        handler! {
            A @ Foo[foo, k] => {
                println!("foo");
                match foo {
                    Foo::This(idx) => k.run::<Foo>(idx * 2),
                    Foo::That(idx) => k.run::<Foo>(idx - 1),
                }
            },
            B @ Bar[_eff, k] => {
                println!("bar");
                k.run::<Bar>("Hello, World!".into())
            },
            C @ effects::Baz[_eff, _k] => {
                println!("baz");
                'x'
            }
        },
    );

    assert_eq!(result, 'o');
}
