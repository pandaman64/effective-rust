#![feature(generators, generator_trait, try_from, never_type)]

#[derive(Debug, PartialEq, Eq)]
struct Eff;

impl eff::Effect for Eff {
    type Output = usize;
}

#[eff::eff(Eff)]
fn foo() {
    eff::perform!(Eff);
}

#[test]
fn test_not_handled() {
    use eff::WithEffect;
    assert_eq!(
        foo().run(|x| x).unwrap_err().uninject().unwrap(),
        (Eff, Default::default())
    );
}
