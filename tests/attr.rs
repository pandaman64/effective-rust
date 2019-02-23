#![feature(generators, generator_trait, try_from, never_type)]
#![feature(trace_macros)]

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
    let e = e
        .handle(|_: Eff| {
            static move || {
                eff::resume!(());
                println!("eff");
            }
        })
        .handle(|_: hoge::Hoge| {
            static move || {
                if false {
                    yield unreachable!()
                }
                println!("hoge");
            }
        });
    e.run(|()| {
        println!("done");
    })
    .unwrap();
}
