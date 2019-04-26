//! Coproduct type of effects

use super::{Effect, TypedContext};

/// A type corresponding to 0
pub enum Zero {}
/// A type corresponding to the next natural number of the argument
pub struct Succ<T>(T);

#[doc(hidden)]
pub struct Wrap<T>(T);

/// Hacky impl for type-level list
impl<T> Effect for Wrap<T> {
    type Output = !;
}

/// The coproduct of effects
pub enum Either<E, Rest>
where
    E: Effect,
{
    A(E, TypedContext<E>),
    B(Rest),
}

/// A trait for constructing a coproduct from an effect and a task context
pub trait Inject<E, Index>
where
    E: Effect,
{
    /// Construct a coproduct from an effect and a task context
    fn inject(effect: E, cx: TypedContext<E>) -> Self;
}

impl<E, Rest> Inject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    #[inline]
    fn inject(effect: E, cx: TypedContext<E>) -> Self {
        Either::A(effect, cx)
    }
}

impl<E, F, Rest, Index> Inject<E, Succ<Index>> for Either<F, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Inject<E, Index>,
{
    #[inline]
    fn inject(effect: E, cx: TypedContext<E>) -> Self {
        Either::B(Rest::inject(effect, cx))
    }
}

/// A trait for destructing a coproduct into an effect and a task context
pub trait Uninject<E, Index>
where
    E: Effect,
{
    /// The other effect types of this coproduct
    type Remainder;

    /// Retrieve an effect and a sender from self if the type matches
    ///
    /// # Errors
    /// If `self` holds an effect of a different type, this method returns an error
    fn uninject(self) -> Result<(E, TypedContext<E>), Self::Remainder>;
}

impl<E, Rest> Uninject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    type Remainder = Rest;

    #[inline]
    fn uninject(self) -> Result<(E, TypedContext<E>), Self::Remainder> {
        match self {
            Either::A(effect, cx) => Ok((effect, cx)),
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
    fn uninject(self) -> Result<(E, TypedContext<E>), Self::Remainder> {
        match self {
            Either::A(effect, cx) => Err(Either::A(effect, cx)),
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
            Either::A(effect, cx) => Target::inject(effect, cx),
            Either::B(rest) => rest.embed(),
        }
    }
}

/// A trait for taking a subset effects out of `Self`
pub trait Subset<Target, Indices> {
    /// The other effect types
    type Remainder;

    /// Take a subset of `Self`
    fn subset(self) -> Result<Target, Self::Remainder>;
}

impl<E, Rest> Subset<!, !> for Either<E, Rest>
where
    E: Effect,
{
    type Remainder = Self;

    #[inline]
    fn subset(self) -> Result<!, Self::Remainder> {
        Err(self)
    }
}

impl<T, E, Rest, HeadIndex, TailIndices>
    Subset<Either<E, Rest>, Either<Wrap<HeadIndex>, TailIndices>> for T
where
    E: Effect,
    T: Uninject<E, HeadIndex>,
    <T as Uninject<E, HeadIndex>>::Remainder: Subset<Rest, TailIndices>,
{
    type Remainder =
        <<T as Uninject<E, HeadIndex>>::Remainder as Subset<Rest, TailIndices>>::Remainder;

    #[inline]
    fn subset(self) -> Result<Either<E, Rest>, Self::Remainder> {
        match self.uninject() {
            Ok((effect, cx)) => Ok(Either::A(effect, cx)),
            Err(rem) => rem.subset().map(Either::B),
        }
    }
}

impl<F, Rest> Either<F, Rest>
where
    F: Effect,
{
    /// Construct `Self` using an effect and a task context
    #[inline]
    pub fn inject<E, Index>(effect: E, cx: TypedContext<E>) -> Self
    where
        E: Effect,
        Self: Inject<E, Index>,
    {
        Inject::inject(effect, cx)
    }

    /// Retrieve an effect and a task context from self if the type matches
    ///
    /// # Errors
    /// If `self` holds an effect of a different type, this method returns an error.
    #[inline]
    pub fn uninject<E, Index>(
        self,
    ) -> Result<(E, TypedContext<E>), <Self as Uninject<E, Index>>::Remainder>
    where
        E: Effect,
        Self: Uninject<E, Index>,
    {
        Uninject::uninject(self)
    }

    /// Apply a handler to one of the effect, leaving the other effects intact
    #[inline]
    pub fn on<Func, E, R, Index>(
        self,
        f: Func,
    ) -> Result<R, <Self as Uninject<E, Index>>::Remainder>
    where
        Func: FnOnce(E, TypedContext<E>) -> R,
        E: Effect,
        Self: Uninject<E, Index>,
    {
        self.uninject().map(|(e, cx)| f(e, cx))
    }

    // TODO: generalize
    /// Apply two handlers to corresponding effect types, leaving the other effects intact
    #[inline]
    pub fn on2<R, Func1, E1, Index1, Func2, E2, Index2>(
        self,
        f1: Func1,
        f2: Func2,
    ) -> Result<R, <<Self as Uninject<E1, Index1>>::Remainder as Uninject<E2, Index2>>::Remainder>
    where
        E1: Effect,
        Func1: FnOnce(E1, TypedContext<E1>) -> R,
        E2: Effect,
        Func2: FnOnce(E2, TypedContext<E2>) -> R,
        Self: Uninject<E1, Index1>,
        <Self as Uninject<E1, Index1>>::Remainder: Uninject<E2, Index2>,
    {
        match self.uninject() {
            Ok((e, cx)) => Ok(f1(e, cx)),
            Err(e) => match e.uninject() {
                Ok((e, cx)) => Ok(f2(e, cx)),
                Err(e) => Err(e),
            },
        }
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
