#![feature(generators)]

use eff::*;

struct Foo;
impl Effect for Foo {
    type Output = ();
}

struct Bar;
impl Effect for Bar {
    type Output = ();
}

#[test]
fn test_compose() {
    let foo = eff! {
        perform!(Foo);
        42_u32
    };

    let bar = eff! {
        perform!(Bar);
        100_u64
    };

    let expr = eff_with_compose!(
        <H @ u32, F @ u64>,
        {
            let hoge = compose!(H, foo);
            let fuga = compose!(F, bar);
            u64::from(hoge) + fuga
        }
    );

    run(
        expr,
        |x| println!("{}", x),
        handler! {
            F @ Foo[_] => {
                println!("foo");
                resume!(())
            },
            B @ Bar[_] => {
                println!("bar");
                resume!(())
            }
        },
    )
}
