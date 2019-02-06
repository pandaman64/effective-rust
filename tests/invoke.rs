#![feature(generators, generator_trait, try_from, never_type)]

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
    let x = invoke!(inner());
    let foo = perform!(Foo);
    x.len() + foo
}

#[test]
fn test_invoke() {
    let e = outer();
    assert_eq!(
        e.handle(|Foo| {
            println!("foo");
            HandlerResult::Resume(42)
        })
        .handle(|Bar(x)| {
            println!("Bar({})", x);
            HandlerResult::Resume(x * 2)
        })
        .run(|x| x),
        44
    );
}
