#![feature(nll, generators, generator_trait, unsized_locals, never_type)]
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
                    match $crate::WithEffect::resume(eff.as_mut(), |x| x) {
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

    fn run<VH>(self, value_handler: VH) -> R
    where
        VH: FnOnce(T) -> R,
    {
        let this = self;
        pin_mut!(this);
        loop {
            match Self::resume(this.as_mut(), |x| x) {
                Resolve::Done(v) => return value_handler(v),
                Resolve::Handled(x) => return x,
                Resolve::NotHandled(_) => panic!("unhandled effect"),
                Resolve::Continue => {}
            }
        }
    }

    fn resume<Derived, A>(derived: Pin<&mut Derived>, accessor: A) -> Resolve<T, R, Self::Effects>
    where
        Derived: WithEffect<T, R>,
        A: Fn(Pin<&mut Derived>) -> Pin<&mut Self>;
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

    fn resume<Derived, A>(derived: Pin<&mut Derived>, accessor: A) -> Resolve<T, R, Self::Effects>
    where
        Derived: WithEffect<T, R>,
        A: Fn(Pin<&mut Derived>) -> Pin<&mut Self>,
    {
        match accessor(derived).inner().resume() {
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

impl<T, R: 'static, WE, E: 'static, I: 'static> WithEffect<T, R> for Handled<WE, R, E, I>
where
    E: Effect,
    WE: WithEffect<T, R>,
    <WE as WithEffect<T, R>>::Effects: coproduct::Uninject<E, I>,
{
    type Effects = <WE::Effects as coproduct::Uninject<E, I>>::Remainder;

    fn resume<Derived, A>(
        mut derived: Pin<&mut Derived>,
        accessor: A,
    ) -> Resolve<T, R, Self::Effects>
    where
        Derived: WithEffect<T, R>,
        A: Fn(Pin<&mut Derived>) -> Pin<&mut Self>,
    {
        match WE::resume(derived.as_mut(), |derived| accessor(derived).inner()) {
            Resolve::Done(v) => Resolve::Done(v),
            Resolve::Continue => Resolve::Continue,
            Resolve::Handled(v) => Resolve::Handled(v),
            Resolve::NotHandled(e) => match coproduct::Uninject::<E, I>::uninject(e) {
                Ok((effect, store)) => {
                    let handler = accessor(derived.as_mut()).handler;
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
