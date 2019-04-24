use super::{Effect, TypedKey};

use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub enum Zero {}
pub struct Succ<T>(PhantomCovariantAlwaysSendSync<T>);

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
    A(E, TypedKey<E>),
    B(Rest),
}

/// A trait for constructing a coproduct from an effect
pub trait Inject<E, Index>
where
    E: Effect,
{
    fn inject(effect: E, key: TypedKey<E>) -> Self;
}

impl<E, Rest> Inject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    #[inline]
    fn inject(effect: E, key: TypedKey<E>) -> Self {
        Either::A(effect, key)
    }
}

impl<E, F, Rest, Index> Inject<E, Succ<Index>> for Either<F, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Inject<E, Index>,
{
    #[inline]
    fn inject(effect: E, key: TypedKey<E>) -> Self {
        Either::B(Rest::inject(effect, key))
    }
}

/// A trait for destructing a coproduct into an effect
pub trait Uninject<E, Index>
where
    E: Effect,
{
    /// The other effect types of this coproduct
    type Remainder;

    /// Retrieve an effect and a sender from self if the type matches
    ///
    /// # Errors
    /// If self holds an effect of a different type, this method returns an error.
    fn uninject(self) -> Result<(E, TypedKey<E>), Self::Remainder>;
}

impl<E, Rest> Uninject<E, Zero> for Either<E, Rest>
where
    E: Effect,
{
    type Remainder = Rest;

    #[inline]
    fn uninject(self) -> Result<(E, TypedKey<E>), Self::Remainder> {
        match self {
            Either::A(effect, key) => Ok((effect, key)),
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
    fn uninject(self) -> Result<(E, TypedKey<E>), Self::Remainder> {
        match self {
            Either::A(effect, key) => Err(Either::A(effect, key)),
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
            Either::A(effect, key) => Target::inject(effect, key),
            Either::B(rest) => rest.embed(),
        }
    }
}

pub trait Subset<Target, Indices> {
    type Remainder;

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
            Ok((effect, key)) => Ok(Either::A(effect, key)),
            Err(rem) => rem.subset().map(Either::B),
        }
    }
}

impl<F, Rest> Either<F, Rest>
where
    F: Effect,
{
    /// Construct `Self` using an effect and a sender
    #[inline]
    pub fn inject<E, Index>(effect: E, key: TypedKey<E>) -> Self
    where
        E: Effect,
        Self: Inject<E, Index>,
    {
        Inject::inject(effect, key)
    }

    /// Retrieve an effect and a sender from self if the type matches
    ///
    /// # Errors
    /// If self holds an effect of a different type, this method returns an error.
    #[inline]
    pub fn uninject<E, Index>(
        self,
    ) -> Result<(E, TypedKey<E>), <Self as Uninject<E, Index>>::Remainder>
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
        Func: FnOnce(E, TypedKey<E>) -> R,
        E: Effect,
        Self: Uninject<E, Index>,
    {
        self.uninject().map(|(e, key)| f(e, key))
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
        Func1: FnOnce(E1, TypedKey<E1>) -> R,
        E2: Effect,
        Func2: FnOnce(E2, TypedKey<E2>) -> R,
        Self: Uninject<E1, Index1>,
        <Self as Uninject<E1, Index1>>::Remainder: Uninject<E2, Index2>,
    {
        match self.uninject() {
            Ok((e, key)) => Ok(f1(e, key)),
            Err(e) => match e.uninject() {
                Ok((e, key)) => Ok(f2(e, key)),
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
