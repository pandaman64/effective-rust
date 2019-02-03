#![feature(generators, generator_trait, try_from)]

#[derive(Debug)]
pub(crate) struct Eff;

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
    let e = foo();
    pin_utils::pin_mut!(e);
    eff::run(
        e,
        |x| x,
        eff::handler! {
            Eff => {
                eff::resume!(())
            },
            hoge::Hoge => {
                eff::resume!(())
            }
        },
    );
}
