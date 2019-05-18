#![feature(generators, never_type)]

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
    use eff::Effectful;

    let e = foo();
    e.handle(eff::handler! {
        v => v,
        Eff, k => eff::perform!(k.resume(println!("eff"))),
    })
    .handle(eff::handler! {
        v => v,
        hoge::Hoge, k => eff::perform!(k.resume(println!("hoge"))),
    })
    .block_on();
}
