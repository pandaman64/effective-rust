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
    let x = perform_from!(inner());
    let foo = perform!(Foo);
    x.len() + foo
}

#[test]
fn test_perform_from() {
    let e = outer();
    assert_eq!(
        e.handle(|Foo| {
            static move || {
                println!("foo");
                resume!(42)
            }
        })
        .handle(|Bar(x)| {
            static move || {
                println!("Bar({})", x);
                resume!(x + 2)
            }
        })
        .run(|x| x)
        .unwrap(),
        44
    );
}
