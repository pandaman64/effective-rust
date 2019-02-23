#![feature(
    nll,
    generators,
    generator_trait,
    never_type,
    specialization,
    core_intrinsics
)]
#![feature(trace_macros)]

use std::cell::RefCell;
use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use pin_utils::{unsafe_pinned, unsafe_unpinned};
use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;
pub use pin_utils::pin_mut;
pub use std::pin as pin_reexport;

pub mod handled;
pub mod unhandled;

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

#[macro_export]
macro_rules! Unhandled {
    () => {
        !
    };
    ($head:ty $(,$tail:ty)* $(,)?) => {
        $crate::unhandled::Either<$head, $crate::Unhandled![$($tail),*]>
    };
}

#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        let store = $crate::Store::default();
        yield $crate::unhandled::Inject::inject($eff, store.clone());
        store.get()
    }};
}

#[macro_export]
macro_rules! perform_from {
    ($eff:expr) => {{
        match $eff {
            eff => {
                $crate::pin_mut!(eff);
                loop {
                    let with_effect = $crate::pin_reexport::Pin::as_mut(&mut eff);
                    match $crate::Effectful::resume(with_effect) {
                        $crate::ComputationState::Done(x) => break x,
                        $crate::ComputationState::Exit(x) => break x,
                        $crate::ComputationState::Handled(h) => {
                            break $crate::handled::Run::run(
                                h,
                                $crate::pin_reexport::Pin::as_mut(&mut eff),
                                |x| x,
                            )
                        }
                        $crate::ComputationState::Unhandled(e) => {
                            yield $crate::unhandled::Embed::embed(e)
                        }
                    }
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! resume {
    ($e:expr) => {{
        let store = $crate::Store::default();
        yield ($crate::Resume::new($e), store.clone());
        store.get()
    }};
}

/// A computational effect that will be resolved to `Output`
pub trait Effect {
    type Output;
}

/// A state of an effectful computation
pub enum ComputationState<T, R, Handled, Unhandled> {
    /// This computation is done
    Done(T),
    /// An effect handler decide to terminate the computation
    Exit(R),
    /// An effect is handled
    Handled(Handled),
    /// There is an unhandled effect
    Unhandled(Unhandled),
}

pub struct Resume<E: Effect, R>(E::Output, PhantomCovariantAlwaysSendSync<R>);

impl<E, R> Resume<E, R>
where
    E: Effect,
{
    pub fn new(v: E::Output) -> Self {
        Resume(v, PhantomData)
    }
}

impl<E, R> Effect for Resume<E, R>
where
    E: Effect,
{
    type Output = R;
}

pub struct Identity<T>(T);

impl<T> Effect for Identity<T> {
    type Output = T;
}

/// An effectful computation
pub trait Effectful<T, R>
where
    Self: Sized,
{
    /// Effect types that is already handled
    type Handled: handled::Run<R>;

    /// Effect types that is not handled yet
    type Unhandled;

    /// Install an effect handler on this value
    #[inline]
    fn handle<E, Index, H, G>(self, handler: H) -> Handled<Self, H, R, E, Index, G>
    where
        E: Effect,
        Self::Unhandled: unhandled::Uninject<E, Index>,
        H: FnMut(E) -> G,
        G: Generator<Yield = (Resume<E, R>, Store<Identity<R>>), Return = R>,
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
    fn run<VH>(self, value_handler: VH) -> Result<R, Self::Unhandled>
    where
        VH: FnOnce(T) -> R,
        Self::Handled: handled::Run<R>,
    {
        use ComputationState::*;

        let this = self;
        pin_mut!(this);
        match this.as_mut().resume() {
            Done(v) => Ok(value_handler(v)),
            Exit(x) => Ok(x),
            Handled(x) => Ok(handled::Run::run(x, this, value_handler)),
            Unhandled(e) => Err(e),
        }
    }

    /// Resume the execution of this expression
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, R, Self::Handled, Self::Unhandled>;
}

impl<T, R, Effects, G> Effectful<T, R> for G
where
    Self: Sized,
    G: Generator<Yield = Effects, Return = T>,
{
    type Handled = !;
    type Unhandled = Effects;

    #[inline]
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, R, Self::Handled, Self::Unhandled> {
        use ComputationState::*;
        use GeneratorState::*;

        match self.resume() {
            Complete(v) => Done(v),
            Yielded(e) => Unhandled(e),
        }
    }
}

/// An effectful computation with a handler for `E` installed.
/// This struct is created by `handle` method.
pub struct Handled<WE, H, R, E, I, G>
where
    E: Effect,
{
    inner: WE,
    handler: H,
    phantom: PhantomCovariantAlwaysSendSync<(R, E, I, G)>,
}

impl<WE, H, R, E, I, G> Handled<WE, H, R, E, I, G>
where
    E: Effect,
{
    unsafe_pinned!(inner: WE);
    unsafe_unpinned!(handler: H);
}

impl<T, R, WE, H, E, I, G> Effectful<T, R> for Handled<WE, H, R, E, I, G>
where
    E: Effect,
    WE: Effectful<T, R>,
    <WE as Effectful<T, R>>::Unhandled: unhandled::Uninject<E, I>,
    H: FnMut(E) -> G,
    G: Generator<Yield = (Resume<E, R>, Store<Identity<R>>), Return = R>,
{
    type Handled = handled::Either<E, G, WE::Handled>;
    type Unhandled = <WE::Unhandled as unhandled::Uninject<E, I>>::Remainder;

    #[inline]
    fn resume(mut self: Pin<&mut Self>) -> ComputationState<T, R, Self::Handled, Self::Unhandled> {
        use ComputationState::*;

        match self.as_mut().inner().resume() {
            Done(v) => Done(v),
            Exit(v) => Exit(v),
            Handled(v) => Handled(handled::Either::B(v)),
            Unhandled(e) => match unhandled::Uninject::uninject(e) {
                Ok((effect, store)) => {
                    let handler = self.handler()(effect);
                    Handled(handled::Inject::inject(handler, store))
                }
                Err(rem) => Unhandled(rem),
            },
        }
    }
}
