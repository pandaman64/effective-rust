#![feature(generators, generator_trait, try_from, never_type)]

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
}

#[test]
fn test_attr() {
    use eff::{HandlerResult, WithEffect};

    let e = foo();
    let e = e
        .handle(|_: Eff| {
            println!("eff");
            HandlerResult::Resume(())
        })
        .handle(|_: hoge::Hoge| {
            println!("hoge");
            HandlerResult::Exit(())
        });
    e.run(|()| {
        println!("done");
    });
}
