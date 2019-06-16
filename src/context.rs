//! A context for a task
//!
//! [TypedContext](struct.TypedContext.html) corresponds to a continuation. You can wake the task
//! via `.waker()` and perform a special effect called [Continue](../struct.Continue.html).
//! `Continue` effect makes the handler wait for the source computation to complete.

use super::{Continue, Effect, Effectful, Poll};

use std::cell::Cell;
use std::fmt;
use std::pin::Pin;
use std::sync::Arc;

use crossbeam_channel::{bounded, Receiver, Sender};

thread_local! {
    static TLS_CX: Cell<Option<&'static Context>> = Cell::new(None);
}

/// The untyped context of the task
///
/// `Context` coordinates how the task gets notified of the next poll
#[derive(Clone)]
pub struct Context {
    notify: Arc<dyn Notify>,
}

impl Context {
    /// Construct a context out of [Notify](trait.Notify.html)
    pub fn from_notify(notify: Arc<dyn Notify>) -> Self {
        Context { notify }
    }

    pub fn notify(&self) -> &Arc<dyn Notify> {
        &self.notify
    }
}

/// A notification object
pub trait Notify: Send + Sync {
    /// Notify the runtime of the wakeup of the task
    fn wake(&self);
}

/// The waker of the task
pub struct Waker<E: Effect> {
    sender: Sender<E::Output>,
    notify: Arc<dyn Notify>,
}

impl<E: Effect> fmt::Debug for Waker<E>
where
    E::Output: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Waker")
            .field("sender", &self.sender)
            .field("notify", &"<...>")
            .finish()
    }
}

impl<E: Effect> Clone for Waker<E> {
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            notify: Arc::clone(&self.notify),
        }
    }
}

impl<E: Effect> Waker<E> {
    fn new(sender: Sender<E::Output>, notify: Arc<dyn Notify>) -> Self {
        Self { sender, notify }
    }

    /// Wake up the task
    pub fn wake(&self, v: E::Output) {
        // set handler result.
        // do not notify the task if send fails because the result will not be used.
        if let Ok(()) = self.sender.send(v) {
            // wake up the task
            self.notify.wake();
        }
    }

    /// Wake up the task if no other wakers have already woken it up
    ///
    /// # Errors
    /// Returns an error if another waker has already woken up the task
    pub fn try_wake(&self, v: E::Output) -> Result<(), E::Output> {
        // set handler result.
        // do not notify the task if send fails because the result will not be used.
        match self.sender.try_send(v) {
            Ok(()) => {
                // wake up the task
                self.notify.wake();
                Ok(())
            }
            Err(e) => Err(e.into_inner()),
        }
    }
}

/// Create a channel for communicating the result of an effect under the context
pub fn channel<E: Effect>(cx: &Context) -> (TypedContext<E>, Receiver<E::Output>) {
    let (sender, receiver) = bounded(1);
    let waker = Waker::new(sender, Arc::clone(&cx.notify));
    (TypedContext(waker), receiver)
}

/// The typed context of the task
pub struct TypedContext<E: Effect>(Waker<E>);

impl<E: Effect> fmt::Debug for TypedContext<E>
where
    E::Output: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("TypedContext").field(&self.0).finish()
    }
}

impl<E: Effect> TypedContext<E> {
    /// Retrieve a waker of the task
    pub fn waker(&self) -> Waker<E> {
        self.0.clone()
    }

    /// Create a [Continue](../struct.Continue.html) effect for the completion of the source computation
    pub fn continuation<R>(self) -> Continue<R> {
        Continue::new()
    }

    /// Wake up the task and create a [Continue](../struct.Continue.html) effect
    ///
    /// This method can be used to immediately resume the source computation
    pub fn resume<R>(self, v: E::Output) -> Continue<R> {
        self.0.wake(v);
        self.continuation()
    }
}

struct SetContextOnDrop(Option<&'static Context>);

impl Drop for SetContextOnDrop {
    fn drop(&mut self) {
        TLS_CX.with(|tls_cx| {
            tls_cx.replace(self.0.take());
        })
    }
}

/// Set the thread-local task context used by the generator-backed effectful computation
pub fn set_task_context<F, R>(cx: &Context, f: F) -> R
where
    F: FnOnce() -> R,
{
    let old_cx = TLS_CX.with(|tls_cx| {
        tls_cx.replace(Some(unsafe {
            &*(cx as *const Context) as &'static Context
        }))
    });
    let _reset_cx = SetContextOnDrop(old_cx);

    f()
}

/// Get the thread-local task context used by the generator-backed effectful computation
///
/// # Panics
/// Panics if the thread-local task is not set
pub fn get_task_context<'a>() -> &'a Context {
    TLS_CX
        .with(|tls_cx| tls_cx.get().take())
        .expect("thread local context must be set")
}

/// Poll the given computation with the thread-local task context
///
/// # Panics
/// Panics if the thread-local task is not set
pub fn poll_with_task_context<C: Effectful>(comp: Pin<&mut C>) -> Poll<C::Output, C::Effect> {
    comp.poll(get_task_context())
}
