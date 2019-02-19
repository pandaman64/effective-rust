#![feature(
    nll,
    generators,
    generator_trait,
    never_type,
    specialization,
    core_intrinsics
)]
#![feature(trace_macros)]

use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use pin_utils::{unsafe_pinned, unsafe_unpinned};
use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;
pub use pin_utils::pin_mut;
pub use std::pin as pin_reexport;

pub mod coproduct;

#[macro_export]
macro_rules! Coproduct {
    () => {
        !
    };
    ($head:ty $(,$tail:ty)* $(,)?) => {
        $crate::coproduct::Either<$head, $crate::Coproduct![$($tail),*]>
    };
}

#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        use $crate::coproduct::Inject;
        let store = $crate::coproduct::Store::default();
        yield $crate::coproduct::Inject::inject($eff, store.clone());
        store.get()
    }};
}

#[macro_export]
macro_rules! invoke {
    ($eff:expr) => {{
        match $eff {
            eff => {
                $crate::pin_mut!(eff);
                loop {
                    let with_effect = $crate::pin_reexport::Pin::as_mut(&mut eff);
                    match $crate::Effectful::resume(with_effect) {
                        $crate::ComputationState::Done(x) => break x,
                        $crate::ComputationState::Exit(x) => break x,
                        $crate::ComputationState::Unhandled(e) => yield $crate::coproduct::Embed::embed(e),
                        $crate::ComputationState::Continue => {}
                    }
                }
            }
        }
    }};
}

/// A computational effect that will be resolved to `Output`
pub trait Effect {
    type Output;
}

/// A state of an effectful computation
pub enum ComputationState<T, R, E> {
    /// An effect is handled and this computation is ready to continue
    Continue,
    /// This computation is done
    Done(T),
    /// An effect handler decide to terminate the computation
    Exit(R),
    /// There is an unhandled effect
    Unhandled(E),
}

/// The result of handling an effect
pub enum HandlerResult<E, R>
where
    E: Effect,
{
    /// The handler decided to terminate the computation and return a value
    Exit(R),
    /// The handler decided to continue the computation with the output
    Resume(E::Output),
}

/// An effectful computation
pub trait Effectful<T, R>
where
    Self: Sized,
{
    /// The coproduct type of effects this expression will produce
    type Effects;

    /// Install an effect handler on this value
    #[inline]
    fn handle<E, Index, H>(self, handler: H) -> Handled<Self, H, R, E, Index>
    where
        E: Effect,
        Self::Effects: coproduct::Uninject<E, Index>,
        H: FnMut(E) -> HandlerResult<E, R>,
    {
        Handled {
            inner: self,
            handler,
            phantom: PhantomData,
        }
    }

    /// Evaluate this expression until 
    /// 1) the computation finishes. In this case, the result will be converted by `effect_handler`;
    /// 2) one of the effect handlers decided to finish the computation; or
    /// 3) an effect is not handled
    ///
    /// # Errors
    /// If the computation raises an effect and it is not handled by the handlers
    /// associated with this expression, an error is returned.
    #[inline]
    fn run<VH>(self, value_handler: VH) -> Result<R, Self::Effects>
    where
        VH: FnOnce(T) -> R,
    {
        use ComputationState::*;

        let this = self;
        pin_mut!(this);
        loop {
            match this.as_mut().resume() {
                Done(v) => return Ok(value_handler(v)),
                Exit(x) => return Ok(x),
                Unhandled(e) => return Err(e),
                Continue => {}
            }
        }
    }

    /// Resume the execution of this expression
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, R, Self::Effects>;
}

impl<T, R, Effects, G> Effectful<T, R> for G
where
    Self: Sized,
    G: Generator<Yield = Effects, Return = T>,
{
    type Effects = Effects;

    #[inline]
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, R, Self::Effects> {
        use GeneratorState::*;
        use ComputationState::*;

        match self.resume() {
            Yielded(e) => Unhandled(e),
            Complete(v) => Done(v),
        }
    }
}
 
/// An effectful computation with a handler for `E` installed.
/// This struct is created by `handle` method.
pub struct Handled<WE, H, R, E, I>
where
    E: Effect,
{
    inner: WE,
    handler: H,
    phantom: PhantomCovariantAlwaysSendSync<(R, E, I)>,
}

impl<WE, H, R, E, I> Handled<WE, H, R, E, I>
where
    E: Effect,
{
    unsafe_pinned!(inner: WE);
    unsafe_unpinned!(handler: H);
}

impl<T, R, WE, H, E, I> Effectful<T, R> for Handled<WE, H, R, E, I>
where
    E: Effect,
    WE: Effectful<T, R>,
    <WE as Effectful<T, R>>::Effects: coproduct::Uninject<E, I>,
    H: FnMut(E) -> HandlerResult<E, R>,
{
    type Effects = <WE::Effects as coproduct::Uninject<E, I>>::Remainder;

    #[inline]
    fn resume(mut self: Pin<&mut Self>) -> ComputationState<T, R, Self::Effects> {
        use ComputationState::*;

        match self.as_mut().inner().resume() {
            Done(v) => Done(v),
            Continue => Continue,
            Exit(v) => Exit(v),
            Unhandled(e) => match coproduct::Uninject::<E, I>::uninject(e) {
                Ok((effect, store)) => match self.handler()(effect) {
                    HandlerResult::Exit(v) => Exit(v),
                    HandlerResult::Resume(output) => {
                        store.set(output);
                        ComputationState::Continue
                    }
                },
                Err(rem) => Unhandled(rem),
            },
        }
    }
}
