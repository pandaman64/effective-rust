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
                    match $crate::WithEffect::resume(with_effect) {
                        $crate::Resolve::Done(x) => break x,
                        $crate::Resolve::Handled(x) => break x,
                        $crate::Resolve::NotHandled(e) => yield $crate::coproduct::Embed::embed(e),
                        $crate::Resolve::Continue => {}
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

impl<T, R, WE, H, E, I> WithEffect<T, R> for Handled<WE, H, R, E, I>
where
    E: Effect,
    WE: WithEffect<T, R>,
    <WE as WithEffect<T, R>>::Effects: coproduct::Uninject<E, I>,
    H: FnMut(E) -> HandlerResult<E, R>,
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
                    match self.handler()(effect) {
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
