#![feature(generators, never_type)]

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

    let result = run(
        expr_with_effect,
        |x| x,
        handler! {
            A @ Foo[foo] => {
                println!("foo");
                match foo {
                    Foo::This(idx) => resume!(idx * 2),
                    Foo::That(idx) => resume!(idx - 1),
                }
            },
            B @ Bar[_eff] => {
                println!("bar");
                resume!("Hello, World!".into())
            },
            C @ effects::Baz[_eff] => {
                println!("baz");
                'x'
            }
        },
    );

    assert_eq!(result, 'o');
}
