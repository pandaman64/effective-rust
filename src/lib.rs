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

use pin_utils::unsafe_pinned;
use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;
pub use pin_utils::pin_mut;

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
        yield Inject::inject($eff, store.clone());
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
                    match eff.as_mut().resume() {
                        Resolve::Done(x) => break x,
                        Resolve::Handled(x) => break x,
                        Resolve::NotHandled(e) => yield e.embed(),
                        Resolve::Continue => {}
                    }
                }
            }
        }
    }};
}

pub trait Effect {
    type Output;
}

pub enum Resolve<T, R, E> {
    Continue,
    Done(T),
    Handled(R),
    NotHandled(E),
}

pub enum HandlerResult<E, R>
where
    E: Effect,
{
    Exit(R),
    Resume(E::Output),
}

pub trait WithEffect<T, R>
where
    Self: Sized,
{
    type Effects;

    #[inline]
    fn handle<E, Index>(self, handler: fn(E) -> HandlerResult<E, R>) -> Handled<Self, R, E, Index>
    where
        E: Effect,
        Self::Effects: coproduct::Uninject<E, Index>,
    {
        Handled {
            inner: self,
            handler,
            phantom: PhantomData,
        }
    }

    #[inline]
    fn run<VH>(self, value_handler: VH) -> Result<R, Self::Effects>
    where
        VH: FnOnce(T) -> R,
    {
        let this = self;
        pin_mut!(this);
        loop {
            match this.as_mut().resume() {
                Resolve::Done(v) => return Ok(value_handler(v)),
                Resolve::Handled(x) => return Ok(x),
                Resolve::NotHandled(e) => return Err(e),
                Resolve::Continue => {}
            }
        }
    }

    fn resume(self: Pin<&mut Self>) -> Resolve<T, R, Self::Effects>;
}

pub struct Unhandled<G> {
    inner: G,
}

impl<G> Unhandled<G> {
    pub fn new(inner: G) -> Self {
        Unhandled { inner }
    }
}

impl<G> Unhandled<G> {
    unsafe_pinned!(inner: G);
}

impl<T, R, Effects, G> WithEffect<T, R> for Unhandled<G>
where
    Self: Sized,
    G: Generator<Yield = Effects, Return = T>,
{
    type Effects = Effects;

    #[inline]
    fn resume(self: Pin<&mut Self>) -> Resolve<T, R, Self::Effects> {
        match self.inner().resume() {
            GeneratorState::Yielded(e) => Resolve::NotHandled(e),
            GeneratorState::Complete(v) => Resolve::Done(v),
        }
    }
}

pub struct Handled<WE, R, E, I>
where
    E: Effect,
{
    inner: WE,
    handler: fn(E) -> HandlerResult<E, R>,
    phantom: PhantomCovariantAlwaysSendSync<I>,
}

impl<WE, R, E, I> Handled<WE, R, E, I>
where
    E: Effect,
{
    unsafe_pinned!(inner: WE);
}

impl<T, R, WE, E, I> WithEffect<T, R> for Handled<WE, R, E, I>
where
    E: Effect,
    WE: WithEffect<T, R>,
    <WE as WithEffect<T, R>>::Effects: coproduct::Uninject<E, I>,
{
    type Effects = <WE::Effects as coproduct::Uninject<E, I>>::Remainder;

    #[inline]
    fn resume(mut self: Pin<&mut Self>) -> Resolve<T, R, Self::Effects> {
        match self.as_mut().inner().resume() {
            Resolve::Done(v) => Resolve::Done(v),
            Resolve::Continue => Resolve::Continue,
            Resolve::Handled(v) => Resolve::Handled(v),
            Resolve::NotHandled(e) => match coproduct::Uninject::<E, I>::uninject(e) {
                Ok((effect, store)) => {
                    let handler = self.handler;
                    match handler(effect) {
                        HandlerResult::Exit(v) => Resolve::Handled(v),
                        HandlerResult::Resume(output) => {
                            store.set(output);
                            Resolve::Continue
                        }
                    }
                }
                Err(rem) => Resolve::NotHandled(rem),
            },
        }
    }
}
