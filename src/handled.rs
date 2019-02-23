use super::{ComputationState, Effect, Identity, Resume, Store, Succ, Wrap, Zero};
use pin_utils::pin_mut;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

/// The coproduct of effects
pub enum Either<E, H, Rest>
where
    E: Effect,
{
    A(H, Store<E>),
    B(Rest),
}

/// A trait for constructing a coproduct from a handler
pub trait Inject<E, H, Index>
where
    E: Effect,
{
    /// Construct `Self` using a handler and a store
    fn inject(handler: H, store: Store<E>) -> Self;
}

impl<E, H, Rest> Inject<E, H, Zero> for Either<E, H, Rest>
where
    E: Effect,
{
    #[inline]
    fn inject(handler: H, store: Store<E>) -> Self {
        Either::A(handler, store)
    }
}

impl<E, H, F, G, Rest, Index> Inject<E, H, Succ<Index>> for Either<F, G, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Inject<E, H, Index>,
{
    #[inline]
    fn inject(handler: H, store: Store<E>) -> Self {
        Either::B(Rest::inject(handler, store))
    }
}

/// A trait for destructing a coproduct into a handler
pub trait Uninject<E, H, Index>
where
    E: Effect,
{
    /// The other handler types of this coproduct
    type Remainder;

    /// Retrieve a handler and a store from self if the type matches
    ///
    /// # Errors
    /// If self holds a handler of a different type, this method returns an error.
    fn uninject(self) -> Result<(H, Store<E>), Self::Remainder>;
}

impl<E, H, Rest> Uninject<E, H, Zero> for Either<E, H, Rest>
where
    E: Effect,
{
    type Remainder = Rest;

    #[inline]
    fn uninject(self) -> Result<(H, Store<E>), Self::Remainder> {
        match self {
            Either::A(handler, store) => Ok((handler, store)),
            Either::B(rest) => Err(rest),
        }
    }
}

impl<E, H, F, G, Rest, Index> Uninject<E, H, Succ<Index>> for Either<F, G, Rest>
where
    E: Effect,
    F: Effect,
    Rest: Uninject<E, H, Index>,
{
    type Remainder = Either<F, G, <Rest as Uninject<E, H, Index>>::Remainder>;

    #[inline]
    fn uninject(self) -> Result<(H, Store<E>), Self::Remainder> {
        match self {
            Either::A(handler, store) => Err(Either::A(handler, store)),
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

impl<E, H, Rest, Target, HeadIndex, TailIndices>
    Embed<Target, Either<Wrap<HeadIndex>, !, TailIndices>> for Either<E, H, Rest>
where
    E: Effect,
    Target: Inject<E, H, HeadIndex>,
    Rest: Embed<Target, TailIndices>,
{
    #[inline]
    fn embed(self) -> Target {
        match self {
            Either::A(handler, store) => Target::inject(handler, store),
            Either::B(rest) => rest.embed(),
        }
    }
}

pub trait Run<R> {
    fn run<T, C, VH>(self, computation: Pin<&mut C>, value_hander: VH) -> R
    where
        C: super::Effectful<T, R>,
        VH: FnOnce(T) -> R;
}

impl<R> Run<R> for ! {
    fn run<T, C, VH>(self, _computation: Pin<&mut C>, _value_hander: VH) -> R
    where
        C: super::Effectful<T, R>,
        VH: FnOnce(T) -> R,
    {
        self
    }
}

impl<E, H, Rest, R> Run<R> for Either<E, H, Rest>
where
    E: Effect,
    H: Generator<Yield = (Resume<E, R>, Store<Identity<R>>), Return = R>,
    Rest: Run<R>,
{
    fn run<T, C, VH>(self, mut computation: Pin<&mut C>, value_handler: VH) -> R
    where
        C: super::Effectful<T, R>,
        VH: FnOnce(T) -> R,
    {
        match self {
            Either::A(handler, store) => {
                pin_mut!(handler);
                match handler.as_mut().resume() {
                    GeneratorState::Complete(v) => v,
                    GeneratorState::Yielded((resume, inner_store)) => {
                        store.set(resume.0);
                        let value = match computation.as_mut().resume() {
                            // I suspect this usage of value handler is correct
                            ComputationState::Done(v) => value_handler(v),
                            ComputationState::Exit(v) => v,
                            ComputationState::Handled(h) => h.run(computation, value_handler),
                            ComputationState::Unhandled(_) => panic!("unhandled effect"),
                        };
                        inner_store.set(value);
                        match handler.resume() {
                            GeneratorState::Complete(v) => v,
                            _ => unreachable!(),
                        }
                    }
                }
            }
            Either::B(rest) => rest.run(computation, value_handler),
        }
    }
}

impl<F, G, Rest> Either<F, G, Rest>
where
    F: Effect,
{
    /// Construct `Self` using a handler and a store
    #[inline]
    pub fn inject<E, H, Index>(handler: H, store: Store<E>) -> Self
    where
        E: Effect,
        Self: Inject<E, H, Index>,
    {
        Inject::inject(handler, store)
    }

    /// Retrieve a handler and a store from self if the type matches
    ///
    /// # Errors
    /// If self holds a handler of a different type, this method returns an error.
    #[inline]
    pub fn uninject<E, H, Index>(
        self,
    ) -> Result<(H, Store<E>), <Self as Uninject<E, H, Index>>::Remainder>
    where
        E: Effect,
        Self: Uninject<E, H, Index>,
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
