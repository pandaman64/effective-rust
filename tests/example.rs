#![feature(generators, generator_trait, try_from, never_type)]

use eff::*;

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

mod effects {
    #[derive(Debug)]
    pub struct Baz;
    impl super::Effect for Baz {
        type Output = !;
    }
}

#[test]
fn test_example() {
    #[eff(Foo, Bar, effects::Baz)]
    fn expr_with_effect() -> char {
        let i1 = perform!(Foo(1));
        let i2 = perform!(Foo(4));
        let s = perform!(Bar);
        if i1 + i2 >= s.len() {
            perform!(effects::Baz);
        }
        s.chars().nth(i1 + i2).unwrap()
    }

    let result = run(
        expr_with_effect(),
        |x| x,
        handler! {
            -> char,
            Foo(idx) => {
                println!("foo");
                resume!(idx + 1)
            },
            Bar => {
                println!("bar");
                resume!("Hello, World!".into())
            },
            effects::Baz => {
                println!("baz");
                exit!('x')
            }
        },
    );

    assert_eq!(result, 'W');
}
