#![feature(nll, generators, generator_trait, never_type)]

use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;
pub use pin_utils::pin_mut;
pub use std::pin as pin_reexport;

pub mod coproduct;

/// A coproduct type of effects
#[macro_export]
macro_rules! Coproduct {
    () => {
        !
    };
    ($head:ty $(,$tail:ty)* $(,)?) => {
        $crate::coproduct::Either<$head, $crate::Coproduct![$($tail),*]>
    };
}

/// Performs an effect, suspending the current computation until the corresponding handler instruct
/// it to resume
#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        let (sender, receiver) = $crate::coproduct::channel();
        yield $crate::coproduct::Inject::inject($eff, sender);
        receiver.get()
    }};
}

/// Runs an effectful computation and retrieve the result of it.
/// When the computation performs an effect, this computation re-perform it as is
#[macro_export]
macro_rules! perform_from {
    ($eff:expr) => {{
        match $eff {
            eff => {
                $crate::pin_mut!(eff);
                loop {
                    let eff = $crate::pin_reexport::Pin::as_mut(&mut eff);
                    match $crate::Effectful::resume(eff) {
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

/// A special effect that represents the resumption of computation
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
pub trait Effectful<T, Effects> {
    /// Install an effect handler
    #[inline]
    fn handle<NewEffects, VH, H, VHC, HC, U>(
        self,
        value_handler: VH,
        handler: H,
    ) -> Handled<NewEffects, Self, T, Effects, VHC, HC, VH, H, U>
    where
        Self: Sized,
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

    /// Convert this computation into one with broader effects by embedding effects
    #[inline]
    fn embed<Indices>(self) -> EmbedEffect<Self, Effects, Indices>
    where
        Self: Sized,
    {
        EmbedEffect(self, PhantomData)
    }

    /// Boxes this computation so that error messages get shorter
    #[inline]
    fn boxed<'a>(self) -> Boxed<'a, T, Effects>
    where
        Self: Sized + 'a,
    {
        Boxed(Box::new(self))
    }

    /// Construct the left variant of Either
    #[inline]
    fn left<R>(self) -> Either<Self, R>
    where
        Self: Sized,
    {
        Either::A(self)
    }

    /// Construct the right variant of Either
    #[inline]
    fn right<L>(self) -> Either<L, Self>
    where
        Self: Sized,
    {
        Either::B(self)
    }

    /// Resume the execution of this expression
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Effects>;
}

/// A boxed effectful computation with type erasured
pub struct Boxed<'a, T, Effects>(Box<dyn Effectful<T, Effects> + 'a>);

impl<'a, T, Effects> Effectful<T, Effects> for Boxed<'a, T, Effects> {
    #[inline]
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, Effects> {
        use std::ops::DerefMut;
        unsafe {
            let this = self.get_unchecked_mut();
            let r = this.0.deref_mut();
            Pin::new_unchecked(r).resume()
        }
    }
}

/// A lazy computation with no effects
pub struct Lazy<F>(Option<F>);

impl<T, F> Effectful<T, !> for Lazy<F>
where
    F: FnOnce() -> T,
{
    #[inline]
    fn resume(self: Pin<&mut Self>) -> ComputationState<T, !> {
        // TODO: verify soundness
        unsafe {
            let this = self.get_unchecked_mut();
            ComputationState::Done(this.0.take().expect("resume after completion")())
        }
    }
}

/// Convert a thunk into a one-shot effectful computation
/// This function is usually used to convert closures into an effect handler
/// (which must be effectful)
#[inline]
pub fn lazy<T, F>(v: F) -> impl Effectful<T, !>
where
    F: FnOnce() -> T,
{
    Lazy(Some(v))
}

/// Convert a value into a one-shot effectful computation that immediately resolves to the value
pub fn pure<T>(v: T) -> impl Effectful<T, !> {
    lazy(move || v)
}

/// An effectful computation that is either of the two underlying ones
pub enum Either<L, R> {
    A(L),
    B(R),
}

impl<T, Effects, L, R> Effectful<T, Effects> for Either<L, R>
where
    L: Effectful<T, Effects>,
    R: Effectful<T, Effects>,
{
    #[inline]
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

/// An effectful computation with broader effects
pub struct EmbedEffect<C, Smaller, Indices>(C, PhantomCovariantAlwaysSendSync<(Smaller, Indices)>);

impl<C, T, Smaller, Larger, Indices> Effectful<T, Larger> for EmbedEffect<C, Smaller, Indices>
where
    C: Effectful<T, Smaller>,
    Smaller: coproduct::Embed<Larger, Indices>,
{
    #[inline]
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
    /// A generator is treated as an effectful computation,
    /// where the return value of the generator corresponds to the result of the computation
    /// and the yielded values to the effects.
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

/// An effectful computation with some effects handled
pub struct Handled<NewEffects, C, T, Effects, VHC, HC, VH, H, U> {
    source: C,
    value_handler: Option<VH>,
    handler: H,
    handler_stack: Vec<(Option<coproduct::Sender<Resume<U>>>, Box<HC>)>,
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
                        let (ref mut sender, ref mut handler) =
                            this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).resume() {
                            Done(v) => {
                                this.handler_stack.pop();
                                match this.handler_stack.last_mut() {
                                    Some((ref mut sender, ref mut _handler)) => {
                                        sender.take().unwrap().set(v);
                                    }
                                    None => return Done(v),
                                }
                            }
                            Effect(coproduct::Either::A(_resume, s)) => {
                                assert!(sender.is_none());
                                *sender = Some(s);
                                this.state = ActiveComputation::Source;
                            }
                            Effect(coproduct::Either::B(e)) => return Effect(e),
                        }
                    }
                    ActiveComputation::ValueHandler(ref mut x) => {
                        match Pin::new_unchecked(x).resume() {
                            Done(v) => match this.handler_stack.last_mut() {
                                None => return Done(v),
                                Some(&mut (ref mut sender, ref mut _handler)) => {
                                    sender.take().unwrap().set(v);
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

/// A pure computation, or computations with no effects
pub trait Pure<T> {
    fn run(self) -> T;
}

impl<C, T> Pure<T> for C
where
    C: Effectful<T, !>,
{
    #[inline]
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
