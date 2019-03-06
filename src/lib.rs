#![feature(nll, generators, generator_trait, never_type)]

use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;
pub use pin_utils::pin_mut;
pub use std::pin as pin_reexport;

pub mod coproduct;

#[macro_export]
macro_rules! Unhandled {
    () => {
        !
    };
    ($head:ty $(,$tail:ty)* $(,)?) => {
        $crate::coproduct::Either<$head, $crate::Unhandled![$($tail),*]>
    };
}

#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        let store = $crate::coproduct::Store::default();
        yield $crate::coproduct::Inject::inject($eff, store.clone());
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
                        $crate::ComputationState::Effect(e) => {
                            yield $crate::coproduct::Embed::embed(e)
                        }
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

pub struct Resume<R>(PhantomCovariantAlwaysSendSync<R>);

impl<R> Effect for Resume<R> {
    type Output = R;
}

/// A state of an effectful computation
pub enum ComputationState<T, Effects> {
    /// This computation is done
    Done(T),
    /// An effect is thrown
    Effect(Effects),
}

/// An effectful computation
pub trait Effectful<T, Effects>
where
    Self: Sized,
{
    #[inline]
    fn handle<NewEffects, VH, H, VHC, HC, U>(
        self,
        value_handler: VH,
        handler: H,
    ) -> Handled<NewEffects, Self, T, Effects, VHC, HC, VH, H, U>
    where
        VH: FnOnce(T) -> VHC,
        H: FnMut(Effects) -> Result<HC, NewEffects>,
        VHC: Effectful<U, NewEffects>,
        HC: Effectful<U, coproduct::Either<Resume<U>, NewEffects>>,
    {
        Handled {
            source: self,
            value_handler: Some(value_handler),
            handler,
            handler_stack: vec![],
            state: ActiveComputation::Source,
            phantom: PhantomData,
        }
    }

    #[inline]
    fn embed<Indices>(self) -> EmbedEffect<Self, Effects, Indices> {
        EmbedEffect(self, PhantomData)
    }

    /// Resume the execution of this expression
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Effects>;
}

pub struct Lazy<F>(Option<F>);

impl<T, F> Effectful<T, !> for Lazy<F>
where
    F: FnOnce() -> T,
{
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, !> {
        // TODO: verify soundness
        unsafe {
            let this = self.get_unchecked_mut();
            ComputationState::Done(this.0.take().expect("resume after completion")())
        }
    }
}

pub fn lazy<T, F>(v: F) -> impl Effectful<T, !>
where
    F: FnOnce() -> T,
{
    Lazy(Some(v))
}

pub fn pure<T>(v: T) -> impl Effectful<T, !> {
    lazy(move || v)
}

pub enum Either<L, R> {
    A(L),
    B(R),
}

impl<T, Effects, L, R> Effectful<T, Effects> for Either<L, R>
where
    L: Effectful<T, Effects>,
    R: Effectful<T, Effects>,
{
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Effects> {
        use Either::*;

        // TODO: verify soundness
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                A(ref mut left) => Pin::new_unchecked(left).resume(),
                B(ref mut right) => Pin::new_unchecked(right).resume(),
            }
        }
    }
}

pub struct EmbedEffect<C, Smaller, Indices>(C, PhantomCovariantAlwaysSendSync<(Smaller, Indices)>);

impl<C, T, Smaller, Larger, Indices> Effectful<T, Larger> for EmbedEffect<C, Smaller, Indices>
where
    C: Effectful<T, Smaller>,
    Smaller: coproduct::Embed<Larger, Indices>,
{
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Larger> {
        use ComputationState::*;

        unsafe {
            match self.map_unchecked_mut(|x| &mut x.0).resume() {
                Done(v) => Done(v),
                Effect(e) => Effect(e.embed()),
            }
        }
    }
}

impl<T, Effects, G> Effectful<T, Effects> for G
where
    Self: Sized,
    G: Generator<Yield = Effects, Return = T>,
{
    #[inline]
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Effects> {
        use ComputationState::*;
        use GeneratorState::*;

        match self.resume() {
            Complete(v) => Done(v),
            Yielded(e) => Effect(e),
        }
    }
}

pub struct Handled<NewEffects, C, T, Effects, VHC, HC, VH, H, U> {
    source: C,
    value_handler: Option<VH>,
    handler: H,
    handler_stack: Vec<(Option<coproduct::Store<Resume<U>>>, Box<HC>)>,
    state: ActiveComputation<VHC>,
    phantom: PhantomCovariantAlwaysSendSync<(T, Effects, NewEffects)>,
}

enum ActiveComputation<VHC> {
    Source,
    Handler,
    ValueHandler(VHC),
}

impl<C, T, OriginalEffects, VHC, HC, VH, H, U, Effects> Effectful<U, Effects>
    for Handled<Effects, C, T, OriginalEffects, VHC, HC, VH, H, U>
where
    VH: FnOnce(T) -> VHC,
    H: FnMut(OriginalEffects) -> Result<HC, Effects>,
    C: Effectful<T, OriginalEffects>,
    VHC: Effectful<U, Effects>,
    HC: Effectful<U, coproduct::Either<Resume<U>, Effects>>,
{
    // I'm not sure whether this inline improves the performance;
    // this method is much larger than I expected
    #[inline]
    fn resume(mut self: Pin<&mut Self>) -> ComputationState<U, Effects> {
        use ComputationState::*;

        // TODO: verify soundness
        unsafe {
            let this = self.as_mut().get_unchecked_mut();
            loop {
                match &mut this.state {
                    ActiveComputation::Source => {
                        match Pin::new_unchecked(&mut this.source).resume() {
                            Done(v) => {
                                this.state = ActiveComputation::ValueHandler(this
                                    .value_handler
                                    .take()
                                    .expect("resume after completion")(
                                    v
                                ));
                            }
                            Effect(e) => match (this.handler)(e) {
                                Ok(x) => {
                                    this.handler_stack.push((None, Box::new(x)));
                                    this.state = ActiveComputation::Handler;
                                }
                                Err(e) => return Effect(e),
                            },
                        }
                    }
                    ActiveComputation::Handler => {
                        let (ref mut store, ref mut handler) =
                            this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).resume() {
                            Done(v) => {
                                this.handler_stack.pop();
                                match this.handler_stack.last_mut() {
                                    Some((ref mut store, ref mut _handler)) => {
                                        store.take().unwrap().set::<!>(v);
                                    }
                                    None => return Done(v),
                                }
                            }
                            Effect(coproduct::Either::A(_resume, s)) => {
                                assert!(store.is_none());
                                *store = Some(s);
                                this.state = ActiveComputation::Source;
                            }
                            Effect(coproduct::Either::B(e)) => return Effect(e),
                        }
                    }
                    ActiveComputation::ValueHandler(ref mut x) => {
                        match Pin::new_unchecked(x).resume() {
                            Done(v) => match this.handler_stack.last_mut() {
                                None => return Done(v),
                                Some(&mut (ref mut store, ref mut _handler)) => {
                                    store.take().unwrap().set::<!>(v);
                                    this.state = ActiveComputation::Handler;
                                }
                            },
                            Effect(e) => return Effect(e),
                        }
                    }
                }
            }
        }
    }
}

pub trait Pure<T> {
    fn run(self) -> T;
}

impl<C, T> Pure<T> for C
where
    C: Effectful<T, !>,
{
    fn run(self) -> T {
        use ComputationState::*;

        let this = self;
        pin_mut!(this);
        match this.resume() {
            Done(v) => v,
            Effect(e) => e,
        }
    }
}
