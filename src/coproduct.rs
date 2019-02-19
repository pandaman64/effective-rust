use super::Effect;
use rich_phantoms::PhantomCovariantAlwaysSendSync;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub enum Zero {}
pub struct Succ<T>(PhantomCovariantAlwaysSendSync<T>);

pub struct Wrap<T>(T);

/// Hacky impl for type-level list
impl<T> Effect for Wrap<T> {
    type Output = !;
}

/// A location to save the output of an effect
/// We can avoid the dynamic allocation of `Rc` once
/// Rust supports "streaming generators" or we use some unsafe code.
#[derive(Debug, PartialEq, Eq)]
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

/// The coproduct of effects
pub enum Either<E, Rest>
where
    E: Effect,
{
    A(E, Store<E>),
    B(Rest),
}

impl<E, Rest> Clone for Either<E, Rest>
where
    E: Effect + Clone,
    Rest: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Either::A(effect, store) => Either::A(effect.clone(), store.clone()),
            Either::B(rest) => Either::B(rest.clone()),
        }
    }
}

impl<E, Rest> fmt::Debug for Either<E, Rest>
where
    E: Effect + fmt::Debug,
    E::Output: fmt::Debug,
    Rest: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Either::A(effect, store) => write!(f, "({:?}, {:?})", effect, store),
            Either::B(rest) => write!(f, "{:?}", rest),
        }
    }
}

impl<E, Rest> PartialEq for Either<E, Rest>
where
    E: Effect + PartialEq,
    E::Output: PartialEq,
    Rest: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        use self::Either::*;

        match (self, other) {
            (A(x, xstore), A(y, ystore)) => x == y && xstore == ystore,
            (B(x), B(y)) => x == y,
            _ => false,
        }
    }
}

impl<E, Rest> Eq for Either<E, Rest>
where
    E: Effect + Eq,
    E::Output: Eq,
    Rest: Eq,
{
}

/// A trait for constructing a coproduct from an effect
pub trait Inject<E, Index>
where
    E: Effect,
{
    /// Construct `Self` using an effect and a store
    fn inject(effect: E, store: Store<E>) -> Self;
}

impl<E, Rest> Inject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    #[inline]
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
    #[inline]
    fn inject(effect: E, store: Store<E>) -> Self {
        Either::B(Rest::inject(effect, store))
    }
}

/// A trait for destructing a coproduct into an effect
pub trait Uninject<E, Index>
where
    E: Effect,
{
    /// The other effect types of this coproduct
    type Remainder;

    /// Retrieve an effect and a store from self if the type matches
    ///
    /// # Errors
    /// If self holds an effect of a different type, this method returns an error.
    fn uninject(self) -> Result<(E, Store<E>), Self::Remainder>;
}

impl<E, Rest> Uninject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    type Remainder = Rest;

    #[inline]
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

    #[inline]
    fn uninject(self) -> Result<(E, Store<E>), Self::Remainder> {
        match self {
            Either::A(effect, store) => Err(Either::A(effect, store)),
            Either::B(rest) => Rest::uninject(rest).map_err(Either::B),
        }
    }
}

/// A trait for embedding self into a wider coproduct type
pub trait Embed<Target, Indices> {
    /// Embed self into the target type
    fn embed(self) -> Target;
}

impl<Target> Embed<Target, !> for ! {
    #[inline]
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
    #[inline]
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
    /// Construct `Self` using an effect and a store
    #[inline]
    pub fn inject<E, Index>(effect: E, store: Store<E>) -> Self
    where
        E: Effect,
        Self: Inject<E, Index>,
    {
        Inject::inject(effect, store)
    }


    /// Retrieve an effect and a store from self if the type matches
    ///
    /// # Errors
    /// If self holds an effect of a different type, this method returns an error.
    #[inline]
    pub fn uninject<E, Index>(
        self,
    ) -> Result<(E, Store<E>), <Self as Uninject<E, Index>>::Remainder>
    where
        E: Effect,
        Self: Uninject<E, Index>,
    {
        Uninject::uninject(self)
    }

    /// Embed self into the target type
    #[inline]
    pub fn embed<Target, Indices>(self) -> Target
    where
        Self: Embed<Target, Indices>,
    {
        Embed::embed(self)
    }
}
