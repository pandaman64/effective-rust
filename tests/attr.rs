#![feature(generators, generator_trait, try_from)]

#[derive(Debug)]
pub(crate) struct Eff();

impl eff::Effect for Eff {
    type Output = ();
}

#[derive(Debug)]
struct Hoge();

impl eff::Effect for Hoge {
    type Output = ();
}

#[eff_attr::eff(Eff, Hoge)]
fn foo() {
    eff::perform!(Eff());
}

#[test]
fn test_attr() {
    eff::run(
        foo(),
        |x| x,
        eff::nonexhaustive_handler! {
            Eff() => {
                eff::new_resume!(())
            }
        },
    );
}
