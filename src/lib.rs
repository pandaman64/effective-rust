#![feature(generator_trait, never_type, stmt_expr_attributes, proc_macro_hygiene)]

use proc_macro_hack::proc_macro_hack;
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use std::thread;

pub use eff_attr::eff;
#[doc(hidden)]
pub use std::pin as pin_reexport;

pub mod context;
pub mod coproduct;
pub mod either;
pub mod embed;
#[cfg(feature = "futures-compat")]
pub mod futures_compat;
pub mod generator;
pub mod handled;
pub mod lazy;
pub mod next_event;
pub mod poll_fn;

pub use context::{poll_with_task_context, Context, Notify, TypedContext, Waker};
pub use generator::from_generator;
pub use lazy::{lazy, pure};

use either::Either;
use embed::EmbedEffect;
use handled::Handled;
use next_event::NextEvent;

/// A coproduct type of effects
#[macro_export]
macro_rules! Coproduct {
    () => {
        !
    };
    (: $tail:ty) => {
        $tail
    };
    ($head:ty $(,$tail:ty)* $(,)?) => {
        $crate::coproduct::Either<$head, $crate::Coproduct![$($tail),*]>
    };
    ($head:ty $(,$middle:ty)* : $tail:ty) => {
        $crate::coproduct::Either<$head, $crate::Coproduct![$($middle),* : $tail]>
    };
}

/// Performs an effect, suspending the current computation until the task gets waken
#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        let eff = $eff;
        let cx = $crate::context::get_task_context();
        let (waker, receiver) = $crate::context::channel(cx);
        yield $crate::Poll::Event($crate::Event::Effect($crate::coproduct::Inject::inject(
            eff, waker,
        )));
        loop {
            if let Ok(v) = receiver.try_recv() {
                break v;
            } else {
                yield $crate::Poll::Pending;
            }
        }
    }};
}

#[macro_export]
macro_rules! reperform_rest {
    ($eff:expr) => {{
        yield $crate::Poll::Event($crate::Event::Effect(
            $crate::coproduct::EmbedRest::embed_rest($eff),
        ));
    }};
}

/// Runs an effectful computation under the current context
///
/// When the computation performs an effect, this computation re-performs it as is
#[macro_export]
macro_rules! perform_from {
    ($eff:expr) => {{
        let mut eff = $eff;
        loop {
            let eff = unsafe { $crate::pin_reexport::Pin::new_unchecked(&mut eff) };
            match $crate::poll_with_task_context(eff) {
                $crate::Poll::Event($crate::Event::Complete(x)) => break x,
                $crate::Poll::Event($crate::Event::Effect(e)) => {
                    // if the computation has no effects, this arm is unreachable
                    #[allow(unreachable_code)]
                    yield $crate::Poll::Event($crate::Event::Effect(
                        $crate::coproduct::Embed::embed(e),
                    ));
                }
                $crate::Poll::Pending => {
                    yield $crate::Poll::Pending;
                }
            }
        }
    }};
}

#[proc_macro_hack]
/// Poll the given computations concurrently
pub use eff_attr::poll;

#[doc(hidden)]
#[macro_export]
macro_rules! handler_impl {
    ($e:expr , ) => {{
        let e: ! = $e;
        e
    }};
    ($e:expr , $effect:pat, $k:pat => $handler:expr; $($effects:pat, $ks:pat => $handlers:expr;)*) => {{
        match $e {
            $crate::coproduct::Either::A($effect, $k) => $handler,
            $crate::coproduct::Either::B(effect) => $crate::handler_impl!(effect , $($effects, $ks => $handlers;)*),
        }
    }};
}

/// Create a handler
///
/// The first arm corresponds to a value handler which is called upon completing the source computation.
///
/// The other arms handles effects performed by the source computation. Each arm takes an effect
/// and a continuation which can wake up the task via `waker()`. See also
/// [TypedContext](context/struct.TypedContext.html).
#[macro_export]
macro_rules! handler {
    ($value:pat => $value_handler:expr $(, $effect:pat, $k:pat => $handler:expr)* $(,)?) => {{
        #[allow(unreachable_code)]
        |arg| $crate::from_generator(static move || {
            if false {
                yield unreachable!();
            }
            match arg {
                $crate::Event::Complete(x) => match x {
                    $value => $value_handler,
                },
                $crate::Event::Effect(e) => $crate::handler_impl!(e , $($effect, $k => $handler;)*),
            }
        })
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

/// An observable event of an effectful computation
#[derive(Debug)]
pub enum Event<T, Effect> {
    /// The computaion is complete
    Complete(T),
    /// An effect has been performed
    Effect(Effect),
}

/// The state of an effectful computation
#[derive(Debug)]
pub enum Poll<T, Effect> {
    /// An event occured
    Event(Event<T, Effect>),
    /// The computation is not ready to continue
    Pending,
}

impl<T, Effect> Poll<T, Effect> {
    pub fn complete(v: T) -> Poll<T, Effect> {
        Poll::Event(Event::Complete(v))
    }

    pub fn effect(e: Effect) -> Poll<T, Effect> {
        Poll::Event(Event::Effect(e))
    }
}

/// An effectful computation
pub trait Effectful {
    /// The type of the final result
    type Output;

    /// The type of the effects this computation will produce
    type Effect;

    /// Takes a value handler and an effect handler and creates an effectful computation with
    /// the effects handled
    #[inline]
    fn handle<H, HC, Effect, I>(self, handler: H) -> Handled<Self, H, HC, Effect, I>
    where
        Self: Sized,
        HC: Effectful,
        H: FnMut(Event<Self::Output, Effect>) -> HC,
        Self::Effect: coproduct::Subset<Effect, I>,
    {
        Handled::new(self, handler)
    }

    /// Creates an effectful computation whose effect is a superset of that of this one
    #[inline]
    fn embed<Target, Indices>(self) -> EmbedEffect<Self, Target, Indices>
    where
        Self: Sized,
    {
        EmbedEffect::new(self)
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
    fn boxed<'a>(self) -> Pin<Box<dyn Effectful<Output = Self::Output, Effect = Self::Effect> + 'a>>
    where
        Self: Sized + 'a,
    {
        Box::pin(self)
    }

    #[inline]
    fn output<T>(self) -> Self
    where
        Self: Effectful<Output = T> + Sized,
    {
        self
    }

    #[inline]
    fn effect<E>(self) -> Self
    where
        Self: Effectful<Effect = E> + Sized,
    {
        self
    }

    #[cfg(feature = "futures-compat")]
    #[inline]
    fn into_future(self) -> futures_compat::future::IntoFuture<Self>
    where
        Self: Effectful<Effect = !> + Sized,
    {
        futures_compat::future::IntoFuture(self)
    }

    /// Run the computation to completion on the current thread
    ///
    /// This method blocks the current thread while waiting on the progress of the computation
    ///
    /// The effect type of this computation must be an empty set (never type) since there is no handler
    #[inline]
    fn block_on(mut self) -> Self::Output
    where
        Self: Sized + Effectful<Effect = !>,
    {
        struct CurrentThreadNotify {
            thread: thread::Thread,
        }

        impl Notify for CurrentThreadNotify {
            fn wake(&self) {
                self.thread.unpark();
            }
        }

        let mut pinned = unsafe { Pin::new_unchecked(&mut self) };

        let cx = Context::from_notify(Arc::new(CurrentThreadNotify {
            thread: thread::current(),
        }));

        loop {
            match pinned.as_mut().poll(&cx) {
                Poll::Event(Event::Complete(v)) => return v,
                Poll::Event(Event::Effect(e)) => e, // unreachable
                Poll::Pending => thread::park(),    // park until wake
            }
        }
    }

    /// Create a computation which polls this until the first event occures.
    #[inline]
    fn next_event(self) -> NextEvent<Self>
    where
        Self: Sized,
    {
        next_event::NextEvent::Computation(self)
    }

    /// Resume the computation to a final value, registering the current task
    /// for wakeup if a handler starts handling an effect performed by the task
    ///
    /// # Return value
    /// This function returns:
    /// - `Poll::Done(v)` with a result `v` if the computation completed successfully
    /// - `Poll::Effect(e)` with an effect `e` if the computation performed a computational effect
    /// - `Poll::Pending` if the computation is not ready to continue because a handler is handling an effect
    ///
    /// Once a computation has completed, clients should not `poll` it again
    ///
    /// When a computation performs an effect, `poll` returns `Poll::Effect(e)` with the effect `e`
    ///
    /// When a handler decides to handle an effect, it will register interest in the result for the
    /// current task. In this case, `poll` returns `Poll::NotReady` until the task gets woken up
    /// with the outcome of the effect.
    ///
    /// # Panics
    /// After the completion of the computation (`poll` returned `Poll::Done`), future calls to `poll`
    /// may panic or cause bad behavior. The `Effectful` trait does not provide any guarantees
    /// about the safety of calling `poll` after the task has finished.
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect>;
}

impl<C> Effectful for &'_ mut C
where
    C: Effectful + Unpin + ?Sized,
{
    type Output = C::Output;
    type Effect = C::Effect;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        C::poll(Pin::new(&mut **self), cx)
    }
}

impl<C> Effectful for Pin<&'_ mut C>
where
    C: Effectful + ?Sized,
{
    type Output = C::Output;
    type Effect = C::Effect;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        C::poll((*self).as_mut(), cx)
    }
}

impl<C> Effectful for Box<C>
where
    C: Effectful + Unpin + ?Sized,
{
    type Output = C::Output;
    type Effect = C::Effect;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        C::poll(Pin::new(&mut **self), cx)
    }
}

impl<C> Effectful for Pin<Box<C>>
where
    C: Effectful + ?Sized,
{
    type Output = C::Output;
    type Effect = C::Effect;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        C::poll((*self).as_mut(), cx)
    }
}
