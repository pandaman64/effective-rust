#![feature(
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
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};

pub use eff_attr::eff;
#[doc(hidden)]
pub use pin_utils::pin_mut;
#[doc(hidden)]
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

/// Performs an effect, suspending the current computation until the task gets waken
#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        match $eff {
            eff => {
                let taker = $crate::gen_taker(&eff);
                let cx = $crate::get_task_context();
                yield $crate::Suspension::Effect($crate::coproduct::Inject::inject(
                    eff,
                    cx.typed(),
                ));
                loop {
                    if let Some(v) = taker(&$crate::get_task_context()) {
                        break v;
                    } else {
                        yield $crate::Suspension::NotReady;
                    }
                }
            }
        }
    }};
}

/// Runs an effectful computation under the current context
///
/// When the computation performs an effect, this computation re-perform it as is
#[macro_export]
macro_rules! perform_from {
    ($eff:expr) => {{
        match $eff {
            eff => {
                $crate::pin_mut!(eff);
                let cx = $crate::get_task_context();
                loop {
                    let eff = $crate::pin_reexport::Pin::as_mut(&mut eff);
                    match $crate::Effectful::poll(eff, $crate::Context::clone(&cx)) {
                        $crate::Poll::Done(x) => break x,
                        $crate::Poll::Effect(e) => {
                            yield $crate::Suspension::Effect($crate::coproduct::Embed::embed(e));
                        }
                        $crate::Poll::NotReady => {
                            yield $crate::Suspension::NotReady;
                        }
                    }
                }
            }
        }
    }};
}

/// An effectful computation block
///
/// The block must contain `perform!` or `perform_from!`
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

/// A special effect representing the continuation of the source computation in handlers
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
pub enum Poll<T, Effect> {
    /// The computation is done
    Done(T),
    /// An effect has been performed
    Effect(Effect),
    /// The computation is not ready to continue
    NotReady,
}

/// The cause of suspension of the computation
pub enum Suspension<Effect> {
    Effect(Effect),
    NotReady,
}

extern "Rust" {
    type Whatever;
}

/// The untyped context of an effectful computation
#[derive(Clone, Debug)]
pub struct Context {
    storage: Arc<Mutex<Option<NonNull<Whatever>>>>,
    thread: Thread,
}

// raw pointer is just an untyped box, so we can implement Send and Sync (really?)
unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Context {
    /// Create a context within the current thread
    pub fn current() -> Self {
        Context {
            storage: Arc::new(Mutex::new(None)),
            thread: thread::current(),
        }
    }

    /// Returns true if the task local storage contains any value
    ///
    /// Returns false if it does not.
    pub fn contains(&self) -> bool {
        self.storage.lock().unwrap().is_some()
    }

    /// Takes the value out of the task local storage
    pub fn take<T>(&self) -> Option<T> {
        unsafe {
            debug!(
                "Context::get: {}, from {:?}",
                type_name::<T>(),
                self.storage
            );

            self.storage
                .lock()
                .unwrap()
                .take()
                .map(|non_null| *(Box::from_raw(non_null.as_ptr() as *mut T)))
        }
    }

    /// Assign a value to the task local storage
    pub fn set<T>(&self, v: T) {
        unsafe {
            debug!("Context::set: {}", type_name::<T>());
            *self.storage.lock().unwrap() = Some(NonNull::new_unchecked(
                Box::into_raw(Box::new(v)) as *mut Whatever,
            ));
        }
    }

    /// Create a typed context referring to the same task local storage
    pub fn typed<E: Effect>(self) -> TypedContext<E> {
        let ptr = Arc::into_raw(self.storage) as *const Mutex<Option<NonNull<E::Output>>>;
        unsafe {
            TypedContext {
                storage: Arc::from_raw(ptr),
                thread: self.thread,
            }
        }
    }
}

/// The typed context of an computation
#[derive(Debug)]
pub struct TypedContext<E: Effect> {
    storage: Arc<Mutex<Option<NonNull<E::Output>>>>,
    thread: Thread,
}

// TODO: This `Clone` impl allows users to create multiple `Continue` and to resume the source
// computation multiple times in a handler, which is an undefined behavior because the second
// resumption leads to a poll after completion
impl<E: Effect> Clone for TypedContext<E> {
    fn clone(&self) -> Self {
        Self {
            storage: Arc::clone(&self.storage),
            thread: self.thread.clone(),
        }
    }
}

// raw pointer is just an untyped box, so we can implement Send and Sync (really?)
unsafe impl<E> Send for TypedContext<E>
where
    E: Effect,
    E::Output: Send,
{
}
unsafe impl<E> Sync for TypedContext<E>
where
    E: Effect,
    E::Output: Sync,
{
}

impl<E: Effect> TypedContext<E> {
    /// Wake up the task associated with this context
    pub fn wake(&self, v: E::Output) {
        // set handler result
        unsafe {
            debug!(
                "TypedContext::set: {} -> {}, to: {:?}",
                type_name::<E>(),
                type_name::<E::Output>(),
                self.storage,
            );
            *self.storage.lock().unwrap() = Some(NonNull::new_unchecked(
                Box::into_raw(Box::new(v)) as *mut E::Output,
            ));
        }

        // unpark task thread
        self.thread.unpark()
    }

    /// Create a `Continue` effect for the completion of the source computation
    pub fn continuation<R>(self) -> Continue<R> {
        Continue::new()
    }

    /// Wake up the task and create a `Continue` effect
    ///
    /// This method can be used to immediately resume the source computation
    pub fn resume<R>(self, v: E::Output) -> Continue<R> {
        self.wake(v);
        self.continuation()
    }
}

/// Helper function for taking the output of an effect of the desired type out of the task local storage
pub fn gen_taker<E>(_e: &E) -> fn(&Context) -> Option<E::Output>
where
    E: Effect,
{
    Context::take::<E::Output>
}

/// An effectful computation
pub trait Effectful {
    /// The type of the final result
    type Output;

    /// The type of the effects this computation will produce
    type Effect;

    /// Takes a value handler and an effect handler and creates an effectful computation with
    /// effects handled
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

    /// Create an effectful computation whose effect is a superset of that of this one
    #[inline]
    fn embed<Target, Indices>(self) -> EmbedEffect<Self, Target, Indices>
    where
        Self: Sized,
    {
        EmbedEffect(self, PhantomData)
    }

    /// Combine this and the other computation with the same signature
    #[inline]
    fn left<R>(self) -> Either<Self, R>
    where
        Self: Sized,
    {
        Either::A(self)
    }

    /// Combine this and the other computation with the same signature
    #[inline]
    fn right<L>(self) -> Either<L, Self>
    where
        Self: Sized,
    {
        Either::B(self)
    }

    /// Create a boxed computation
    ///
    /// This function can be used to erase `Self` type
    #[inline]
    fn boxed<'a>(self) -> Boxed<'a, Self::Output, Self::Effect>
    where
        Self: Sized + 'a,
    {
        Boxed(Box::new(self))
    }

    /// Run this computation to completion on the current thread
    ///
    /// This method blocks the current thread while waiting for the progress of the computation
    ///
    /// The effect type of this computation must be an empty set (never type) since there is no handler
    #[inline]
    fn block_on(self) -> Self::Output
    where
        Self: Sized + Effectful<Effect = !>,
    {
        use Poll::*;

        let this = self;
        pin_mut!(this);

        let cx = Context::current();

        loop {
            match this.as_mut().poll(cx.clone()) {
                Done(v) => return v,
                Effect(e) => e,             // unreachable
                NotReady => thread::park(), // park until wake
            }
        }
    }

    /// Resume the computation to a final value, registering the current task
    /// for wakeup if a handler starts handling an effect performed by the task
    ///
    /// # Return value
    /// This function returns:
    /// - `Poll::Done(v)` with a result `v` if the computation completed successfully
    /// - `Poll::Effect(e)` with an effect `e` if the computation performed a computational effect
    /// - `Poll::NotReady` if the computation is not ready to continue because a handler is handling an effect
    ///
    /// Once a computation has completed, clients should not `poll` it again
    ///
    /// When a computation performs an effect, `poll` returns `Poll::Effect(e)` with the effect `e`
    ///
    /// When a handler decides to handle an effect, it will register the interest in the result for
    /// the current task. In this case, `poll` returns `Poll::NotReady` until the task gets woken up
    /// with the outcome of the effect.
    fn poll(self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect>;
}

/// A boxed effectful computation with type erased
pub struct Boxed<'a, Output, Effect>(Box<dyn Effectful<Output = Output, Effect = Effect> + 'a>);

impl<'a, Output, Effect> Effectful for Boxed<'a, Output, Effect> {
    type Output = Output;
    type Effect = Effect;

    /// Poll the inner computation
    #[inline]
    fn poll(self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use std::ops::DerefMut;
        unsafe {
            let this = self.get_unchecked_mut();
            let r = this.0.deref_mut();
            Pin::new_unchecked(r).poll(cx)
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

    /// Execute the computation
    ///
    /// # Panics
    /// If the task is polled again after completion, this method panics
    #[inline]
    fn poll(self: Pin<&mut Self>, _cx: Context) -> Poll<Self::Output, Self::Effect> {
        unsafe {
            let this = self.get_unchecked_mut();
            Poll::Done(this.0.take().expect("poll after completion")())
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

/// An effectful computation created by `embed()` combinator
///
/// `EmbedEffect` is used to "widen" the effect that the task will perform
pub struct EmbedEffect<C, Target, Indices>(C, PhantomData<(Target, Indices)>);

impl<C, Target, Indices> Effectful for EmbedEffect<C, Target, Indices>
where
    C: Effectful,
    C::Effect: coproduct::Embed<Target, Indices>,
{
    type Output = C::Output;
    type Effect = Target;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use coproduct::Embed;
        use Poll::*;

        match unsafe { self.map_unchecked_mut(|this| &mut this.0) }.poll(cx) {
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
    fn poll(self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use Either::*;
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                A(ref mut left) => Pin::new_unchecked(left).poll(cx),
                B(ref mut right) => Pin::new_unchecked(right).poll(cx),
            }
        }
    }
}

thread_local! {
    static TLS_CX: Cell<Option<Context>> = Cell::new(None);
}

struct SetOnDrop(Option<Context>);

impl Drop for SetOnDrop {
    fn drop(&mut self) {
        TLS_CX.with(|tls_cx| {
            tls_cx.replace(self.0.take());
        })
    }
}

/// Set the thread-local task context used by generator-backed effectful computation
pub fn set_task_context<F, R>(cx: Context, f: F) -> R
where
    F: FnOnce() -> R,
{
    let old_cx = TLS_CX.with(|tls_cx| tls_cx.replace(Some(cx)));
    let _reset = SetOnDrop(old_cx);
    f()
}

/// Get the thread-local task context used by generator-backed effectful computation
pub fn get_task_context() -> Context {
    TLS_CX
        .with(|tls_cx| {
            let cx = tls_cx.replace(None).take();
            tls_cx.set(cx.clone());
            cx
        })
        .expect("thread local context must be set")
}

struct GenEffectful<G>(G);

/// Create an effectful computation which wraps a generator
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

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use GeneratorState::*;
        use Poll::*;

        unsafe {
            let this = &mut self.get_unchecked_mut().0;
            set_task_context(cx, || match Pin::new_unchecked(this).resume() {
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
    fn poll(mut self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use Poll::*;

        // TODO: verify soundness
        unsafe {
            let this = self.as_mut().get_unchecked_mut();
            loop {
                match &mut this.state {
                    ActiveComputation::Source => {
                        match Pin::new_unchecked(&mut this.source).poll(cx.clone()) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    this.state = ActiveComputation::ValueHandler(this
                                        .value_handler
                                        .take()
                                        .expect("poll after completion")(
                                        v
                                    ));
                                } else {
                                    // fulfill Continue<Output> effect
                                    this.state = ActiveComputation::Handler;
                                    cx.set(v);
                                }
                            }
                            Effect(e) => match (this.handler)(e) {
                                Ok(x) => {
                                    this.handler_stack.push(Box::new(x));
                                    this.state = ActiveComputation::Handler;
                                }
                                Err(e) => return Effect(e),
                            },
                            NotReady => return NotReady,
                        }
                    }
                    ActiveComputation::Handler => {
                        let handler = this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).poll(cx.clone()) {
                            Done(v) => {
                                this.handler_stack.pop();

                                // the last handler
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    cx.set(v);
                                }
                            }
                            Effect(coproduct::Either::A(_, _)) => {
                                // continue the original computation
                                this.state = ActiveComputation::Source;

                                // if the handler has already waken the task, continue the computation
                                // otherwise, wait until wake() is called
                                if !cx.contains() {
                                    return NotReady;
                                }
                            }
                            Effect(coproduct::Either::B(e)) => return Effect(e),
                            NotReady => return NotReady,
                        }
                    }
                    ActiveComputation::ValueHandler(ref mut x) => {
                        match Pin::new_unchecked(x).poll(cx.clone()) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    cx.set(v);
                                    this.state = ActiveComputation::Handler;
                                }
                            }
                            Effect(e) => return Effect(e),
                            NotReady => return NotReady,
                        }
                    }
                }
            }
        }
    }
}
