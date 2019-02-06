use super::Effect;
use rich_phantoms::PhantomCovariantAlwaysSendSync;

use std::cell::RefCell;
use std::rc::Rc;

pub enum Zero {}
pub struct Succ<T>(PhantomCovariantAlwaysSendSync<T>);

pub struct Wrap<T>(T);

/// Hacky impl for type-level list
impl<T> Effect for Wrap<T> {
    type Output = !;
}

pub struct Store<E>
where
    E: Effect,
{
    inner: Rc<RefCell<Option<E::Output>>>,
}

impl<E> std::default::Default for Store<E>
where
    E: Effect,
{
    fn default() -> Self {
        Store {
            inner: Default::default(),
        }
    }
}

impl<E> Clone for Store<E>
where
    E: Effect,
{
    fn clone(&self) -> Self {
        Store {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<E> Store<E>
where
    E: Effect,
{
    pub fn get(&self) -> E::Output {
        self.inner.borrow_mut().take().unwrap()
    }

    pub fn set(&self, v: E::Output) {
        *self.inner.borrow_mut() = Some(v);
    }
}

pub enum Either<E, Rest>
where
    E: Effect,
{
    A(E, Store<E>),
    B(Rest),
}

pub trait Inject<E, Index>
where
    E: Effect,
{
    fn inject(effect: E, store: Store<E>) -> Self;
}

impl<E, Rest> Inject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    fn inject(effect: E, store: Store<E>) -> Self {
        Either::A(effect, store)
    }
}

impl<E, F, Rest, Index> Inject<E, Succ<Index>> for Either<F, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Inject<E, Index>,
{
    fn inject(effect: E, store: Store<E>) -> Self {
        Either::B(Rest::inject(effect, store))
    }
}

pub trait Uninject<E, Index>
where
    E: Effect,
{
    type Remainder;

    fn uninject(self) -> Result<(E, Store<E>), Self::Remainder>;
}

impl<E, Rest> Uninject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    type Remainder = Rest;

    fn uninject(self) -> Result<(E, Store<E>), Self::Remainder> {
        match self {
            Either::A(effect, store) => Ok((effect, store)),
            Either::B(rest) => Err(rest),
        }
    }
}

impl<E, F, Rest, Index> Uninject<E, Succ<Index>> for Either<F, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Uninject<E, Index>,
{
    type Remainder = Either<F, <Rest as Uninject<E, Index>>::Remainder>;

    fn uninject(self) -> Result<(E, Store<E>), Self::Remainder> {
        match self {
            Either::A(effect, store) => Err(Either::A(effect, store)),
            Either::B(rest) => Rest::uninject(rest).map_err(Either::B),
        }
    }
}

pub trait Embed<Target, Indices> {
    fn embed(self) -> Target;
}

impl<Target> Embed<Target, !> for ! {
    fn embed(self) -> Target {
        unreachable!()
    }
}

impl<E, Rest, Target, HeadIndex, TailIndices> Embed<Target, Either<Wrap<HeadIndex>, TailIndices>>
    for Either<E, Rest>
where
    E: Effect,
    Target: Inject<E, HeadIndex>,
    Rest: Embed<Target, TailIndices>,
{
    fn embed(self) -> Target {
        match self {
            Either::A(effect, store) => Target::inject(effect, store),
            Either::B(rest) => rest.embed(),
        }
    }
}

impl<F, Rest> Either<F, Rest>
where
    F: Effect,
{
    pub fn inject<E, Index>(effect: E, store: Store<E>) -> Self
    where
        E: Effect,
        Self: Inject<E, Index>,
    {
        Inject::inject(effect, store)
    }

    pub fn uninject<E, Index>(
        self,
    ) -> Result<(E, Store<E>), <Self as Uninject<E, Index>>::Remainder>
    where
        E: Effect,
        Self: Uninject<E, Index>,
    {
        Uninject::uninject(self)
    }

    pub fn embed<Target, Indices>(self) -> Target
    where
        Self: Embed<Target, Indices>,
    {
        Embed::embed(self)
    }
}
