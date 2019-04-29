#![feature(generators, generator_trait, never_type)]

use eff::*;

#[derive(Debug)]
struct Foo;

impl Effect for Foo {
    type Output = usize;
}

#[derive(Debug)]
struct Bar(usize);

impl Effect for Bar {
    type Output = usize;
}

#[eff(Bar)]
fn inner() -> String {
    perform!(Bar(10)).to_string()
}

#[eff(Foo, Bar)]
fn outer() -> usize {
    let x = perform_from!(inner());
    let foo = perform!(Foo);
    x.len() + foo
}

#[test]
fn test_perform_from() {
    let e = outer();
    assert_eq!(
        e.handle(handler! {
            x => x,
            Foo, k => {
                println!("foo");
                perform!(k.resume(42))
            }
        })
        .handle(handler! {
            x => x,
            Bar(x), k => {
                println!("Bar({})", x);
                perform!(k.resume(x + 2))
            }
        })
        .block_on(),
        44
    );
}
