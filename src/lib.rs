#![feature(
    nll,
    generators,
    generator_trait,
    never_type,
    core_intrinsics,
    extern_types
)]

use log::debug;
use std::cell::Cell;
use std::intrinsics::type_name;
use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::ptr::NonNull;
use std::rc::Rc;

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
        match $eff {
            eff => {
                let getter = $crate::gen_getter(&eff);
                let key = $crate::get_key();
                yield $crate::Suspension::Effect($crate::coproduct::Inject::inject(
                    eff,
                    key.typed(),
                ));
                getter(&$crate::get_key())
            }
        }
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
                let key = $crate::get_key();
                loop {
                    let eff = $crate::pin_reexport::Pin::as_mut(&mut eff);
                    match $crate::Effectful::resume(eff, std::rc::Rc::clone(&key)) {
                        $crate::ComputationState::Done(x) => break x,
                        $crate::ComputationState::Effect(e) => {
                            yield $crate::Suspension::Effect($crate::coproduct::Embed::embed(e));
                        }
                        $crate::ComputationState::NotReady => {
                            yield $crate::Suspension::NotReady;
                        }
                    }
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! effectful {
    ($($tts:tt)*) => {{
        $crate::from_generator(static move || {
            $($tts)*
        })
    }};
}

/// A computational effect that will be resolved to `Output`
pub trait Effect {
    type Output;
}

#[derive(Debug)]
pub struct Continue<R>(PhantomData<R>);

impl<R> Effect for Continue<R> {
    type Output = R;
}

impl<R> Continue<R> {
    fn new() -> Self {
        Continue(PhantomData)
    }
}

/// A state of an effectful computation
pub enum ComputationState<T, Effect> {
    /// This computation is done
    Done(T),
    /// An effect is thrown
    Effect(Effect),
    /// A handler is waiting for completion of original computation
    NotReady,
}

pub enum Suspension<Effect> {
    Effect(Effect),
    NotReady,
}

// TODO: verify soundness
extern "Rust" {
    type Whatever;
}

#[derive(Debug)]
#[repr(transparent)]
pub struct LocalKey {
    storage: Cell<Option<NonNull<Whatever>>>,
}

impl LocalKey {
    fn new() -> Self {
        Self {
            storage: Cell::new(None),
        }
    }

    pub fn contains(&self) -> bool {
        self.storage.get().is_some()
    }

    pub fn get<T>(&self) -> T {
        unsafe {
            debug!(
                "LocalKey::get: {}, from {:?}",
                type_name::<T>(),
                self.storage.get()
            );
            *(Box::from_raw(
                self.storage
                    .replace(None)
                    .expect("argument must be set")
                    .as_ptr() as *mut T,
            ))
        }
    }

    pub fn set<T>(&self, v: T) {
        unsafe {
            debug!("LocalKey::set: {}", type_name::<T>());
            self.storage.set(Some(NonNull::new_unchecked(
                Box::into_raw(Box::new(v)) as *mut Whatever
            )));
        }
    }

    pub fn typed<E: Effect>(self: Rc<Self>) -> TypedKey<E> {
        let ptr = Rc::into_raw(self) as *const Cell<Option<NonNull<E::Output>>>;
        unsafe {
            TypedKey {
                storage: Rc::from_raw(ptr),
            }
        }
    }
}

pub struct TypedKey<E: Effect> {
    storage: Rc<Cell<Option<NonNull<E::Output>>>>,
}

impl<E: Effect> TypedKey<E> {
    pub fn wake(&self, v: E::Output) {
        unsafe {
            debug!(
                "TypedKey::set: {} -> {}, to: {:?}",
                type_name::<E>(),
                type_name::<E::Output>(),
                self.storage.get(),
            );
            self.storage.set(dbg!(Some(NonNull::new_unchecked(
                Box::into_raw(Box::new(v)) as *mut E::Output
            ))));
        }
    }

    pub fn continuation<R>(self) -> Continue<R> {
        Continue::new()
    }

    pub fn resume<R>(self, v: E::Output) -> Continue<R> {
        self.wake(v);
        self.continuation()
    }

    /*
    pub unsafe fn untyped(self: Rc<Self>) -> Rc<LocalKey> {
        let ptr = Rc::into_raw(self) as *const LocalKey;
        unsafe { Rc::from_raw(ptr) }
    }*/
}

pub fn gen_getter<E>(_e: &E) -> fn(&LocalKey) -> E::Output
where
    E: Effect,
{
    LocalKey::get::<E::Output>
}

/// An effectful computation
pub trait Effectful {
    type Output;
    type Effect;

    #[inline]
    fn handle<H, HC, VH, VHC, NewEffect>(
        self,
        value_handler: VH,
        handler: H,
    ) -> Handled<Self, H, HC, VH, VHC>
    where
        Self: Sized,
        VH: FnOnce(Self::Output) -> VHC,
        H: FnMut(Self::Effect) -> Result<HC, NewEffect>,
    {
        Handled {
            source: self,
            value_handler: Some(value_handler),
            handler,
            handler_stack: vec![],
            state: ActiveComputation::Source,
        }
    }

    #[inline]
    fn embed<Target, Indices>(self) -> EmbedEffect<Self, Target, Indices>
    where
        Self: Sized,
    {
        EmbedEffect(self, PhantomData)
    }

    #[inline]
    fn left<R>(self) -> Either<Self, R>
    where
        Self: Sized,
    {
        Either::A(self)
    }

    #[inline]
    fn right<L>(self) -> Either<L, Self>
    where
        Self: Sized,
    {
        Either::B(self)
    }

    #[inline]
    fn boxed<'a>(self) -> Boxed<'a, Self::Output, Self::Effect>
    where
        Self: Sized + 'a,
    {
        Boxed(Box::new(self))
    }

    #[inline]
    fn block_on(self) -> Self::Output
    where
        Self: Sized + Effectful<Effect = !>,
    {
        use ComputationState::*;

        let this = self;
        pin_mut!(this);

        let local_key = Rc::new(LocalKey::new());

        loop {
            match this.as_mut().resume(Rc::clone(&local_key)) {
                Done(v) => return v,
                Effect(e) => e, // unreachable
                NotReady => unimplemented!("current thread must be parked until wake()"),
            }
        }
    }

    /// Resume the execution of this expression
    fn resume(
        self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect>;
}

/// A boxed effectful computation with type erasured
pub struct Boxed<'a, Output, Effect>(Box<dyn Effectful<Output = Output, Effect = Effect> + 'a>);

impl<'a, Output, Effect> Effectful for Boxed<'a, Output, Effect> {
    type Output = Output;
    type Effect = Effect;

    #[inline]
    fn resume(
        self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
        use std::ops::DerefMut;
        unsafe {
            let this = self.get_unchecked_mut();
            let r = this.0.deref_mut();
            Pin::new_unchecked(r).resume(key)
        }
    }
}

/// A lazy computation with no effects
pub struct Lazy<F>(Option<F>);

impl<T, F> Effectful for Lazy<F>
where
    F: FnOnce() -> T,
{
    type Output = T;
    type Effect = !;

    #[inline]
    fn resume(
        self: Pin<&mut Self>,
        _key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
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
pub fn lazy<T, F>(v: F) -> impl Effectful<Output = T, Effect = !>
where
    F: FnOnce() -> T,
{
    Lazy(Some(v))
}

/// Convert a value into a one-shot effectful computation that immediately resolves to the value
pub fn pure<T>(v: T) -> impl Effectful<Output = T, Effect = !> {
    lazy(move || v)
}

pub struct EmbedEffect<C, Target, Indices>(C, PhantomData<(Target, Indices)>);

impl<C, Target, Indices> Effectful for EmbedEffect<C, Target, Indices>
where
    C: Effectful,
    C::Effect: coproduct::Embed<Target, Indices>,
{
    type Output = C::Output;
    type Effect = Target;

    fn resume(
        self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
        use coproduct::Embed;
        use ComputationState::*;

        match unsafe { self.map_unchecked_mut(|this| &mut this.0) }.resume(key) {
            Done(v) => Done(v),
            Effect(e) => Effect(e.embed()),
            NotReady => NotReady,
        }
    }
}

/// An effectful computation that is either of the two underlying ones
pub enum Either<L, R> {
    A(L),
    B(R),
}

impl<Output, Effect, L, R> Effectful for Either<L, R>
where
    L: Effectful<Output = Output, Effect = Effect>,
    R: Effectful<Output = Output, Effect = Effect>,
{
    type Output = Output;
    type Effect = Effect;

    #[inline]
    fn resume(
        self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
        use Either::*;

        // TODO: verify soundness
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                A(ref mut left) => Pin::new_unchecked(left).resume(key),
                B(ref mut right) => Pin::new_unchecked(right).resume(key),
            }
        }
    }
}

thread_local! {
    static TLS_KEY: Cell<Option<Rc<LocalKey>>> = Cell::new(None);
}

struct SetOnDrop(Option<Rc<LocalKey>>);

impl Drop for SetOnDrop {
    fn drop(&mut self) {
        TLS_KEY.with(|tls_key| {
            tls_key.replace(self.0.take());
        })
    }
}

pub fn set_key<F, R>(value: Rc<LocalKey>, f: F) -> R
where
    F: FnOnce() -> R,
{
    let old_value = TLS_KEY.with(|tls_key| tls_key.replace(Some(value)));
    let _reset = SetOnDrop(old_value);
    f()
}

pub fn get_key() -> Rc<LocalKey> {
    let key = TLS_KEY
        .with(|tls_key| {
            let key = tls_key.replace(None).take();
            tls_key.set(key.clone());
            key
        })
        .expect("local key must be set");
    key
}

pub struct GenEffectful<G>(G);

pub fn from_generator<G, Output, Effect>(x: G) -> impl Effectful<Output = Output, Effect = Effect>
where
    G: Generator<Return = Output, Yield = Suspension<Effect>>,
{
    GenEffectful(x)
}

impl<G, Output, Effect> Effectful for GenEffectful<G>
where
    G: Generator<Return = Output, Yield = Suspension<Effect>>,
{
    type Output = Output;
    type Effect = Effect;

    /// A generator is treated as an effectful computation,
    /// where the return value of the generator corresponds to the result of the computation
    /// and the yielded values to the effects.
    #[inline]
    fn resume(
        self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
        use ComputationState::*;
        use GeneratorState::*;

        unsafe {
            let this = &mut self.get_unchecked_mut().0;
            set_key(key, || match Pin::new_unchecked(this).resume() {
                Complete(v) => Done(v),
                Yielded(Suspension::Effect(e)) => Effect(e),
                Yielded(Suspension::NotReady) => NotReady,
            })
        }
    }
}

/// An effectful computation with some effects handled
pub struct Handled<C, H, HC, VH, VHC> {
    source: C,
    value_handler: Option<VH>,
    handler: H,
    handler_stack: Vec<Box<HC>>,
    state: ActiveComputation<VHC>,
}

enum ActiveComputation<VHC> {
    Source,
    Handler,
    ValueHandler(VHC),
}

impl<C, Output, Effect, H, HC, VH, VHC, NewOutput, NewEffect> Effectful
    for Handled<C, H, HC, VH, VHC>
where
    C: Effectful<Output = Output, Effect = Effect>,
    VH: FnOnce(Output) -> VHC,
    H: FnMut(Effect) -> Result<HC, NewEffect>,
    VHC: Effectful<Output = NewOutput, Effect = NewEffect>,
    HC: Effectful<Output = NewOutput, Effect = coproduct::Either<Continue<Output>, NewEffect>>,
{
    type Output = NewOutput;
    type Effect = NewEffect;

    // I'm not sure whether this inline improves the performance;
    // this method is much larger than I expected
    #[inline]
    fn resume(
        mut self: Pin<&mut Self>,
        key: Rc<LocalKey>,
    ) -> ComputationState<Self::Output, Self::Effect> {
        use ComputationState::*;

        // TODO: verify soundness
        unsafe {
            let this = self.as_mut().get_unchecked_mut();
            loop {
                match &mut this.state {
                    ActiveComputation::Source => {
                        match Pin::new_unchecked(&mut this.source).resume(Rc::clone(&key)) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    this.state = ActiveComputation::ValueHandler(this
                                        .value_handler
                                        .take()
                                        .expect("resume after completion")(
                                        v
                                    ));
                                } else {
                                    // fulfill Continue<Output> effect
                                    this.state = ActiveComputation::Handler;
                                    key.set(v);
                                }
                            }
                            Effect(e) => match (this.handler)(e) {
                                Ok(x) => {
                                    this.handler_stack.push(Box::new(x));
                                    this.state = ActiveComputation::Handler;
                                }
                                Err(e) => return Effect(e),
                            },
                            NotReady => return NotReady, // TODO: correct?
                        }
                    }
                    ActiveComputation::Handler => {
                        let handler = this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).resume(Rc::clone(&key)) {
                            Done(v) => {
                                this.handler_stack.pop();

                                // the last handler
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    key.set(v);
                                }
                            }
                            Effect(coproduct::Either::A(_, _)) => {
                                // continue the original computation
                                this.state = ActiveComputation::Source;

                                // if the handler has already waken the task, continue the computation
                                // otherwise, wait until wake() is called
                                if !key.contains() {
                                    return NotReady;
                                }
                            }
                            Effect(coproduct::Either::B(e)) => return Effect(e),
                            NotReady => return NotReady, // TODO: correct?
                        }
                    }
                    ActiveComputation::ValueHandler(ref mut x) => {
                        match Pin::new_unchecked(x).resume(Rc::clone(&key)) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    key.set(v);
                                    this.state = ActiveComputation::Handler;
                                }
                            }
                            Effect(e) => return Effect(e),
                            NotReady => return NotReady, // TODO: correct?
                        }
                    }
                }
            }
        }
    }
}
