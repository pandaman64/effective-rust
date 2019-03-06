#![feature(generators, generator_trait, never_type)]

#[derive(Debug)]
struct Eff;

impl eff::Effect for Eff {
    type Output = ();
}

mod hoge {
    #[derive(Debug)]
    pub struct Hoge;

    impl eff::Effect for Hoge {
        type Output = ();
    }
}

#[eff_attr::eff(Eff, hoge::Hoge)]
fn foo() {
    eff::perform!(Eff);
    eff::perform!(hoge::Hoge);
    println!("this doesn't print");
}

#[test]
fn test_attr() {
    use eff::{Effectful, Pure};

    let e = foo();
    e.handle(
        |()| eff::pure(println!("done 1")).embed(),
        |e| {
            e.on(|Eff, store| {
                static move || {
                    eff::perform!(store.set(println!("eff")));
                }
            })
        },
    )
    .handle(
        |()| eff::pure(println!("done 2")),
        |e| {
            e.on(|hoge::Hoge, store| {
                static move || {
                    eff::perform!(store.set(println!("hoge")));
                }
            })
        },
    )
    .run();
}
