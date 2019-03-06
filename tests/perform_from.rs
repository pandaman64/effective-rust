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
        e.handle(
            |x| pure(x).embed(),
            |e| {
                e.on(|Foo, store| {
                    static move || {
                        println!("foo");
                        perform!(store.set(42))
                    }
                })
            }
        )
        .handle(
            |x| pure(x).embed(),
            |e| {
                e.on(|Bar(x), store| {
                    static move || {
                        println!("Bar({})", x);
                        perform!(store.set(x + 2))
                    }
                })
            }
        )
        .run(),
        44
    );
}
