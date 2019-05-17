//! A context for a task
//!
//! [TypedContext](struct.TypedContext.html) corresponds to a continuation. You can wake the task
//! via `.waker()` and perform a special effect called [Continue](../struct.Continue.html).
//! `Continue` effect makes the handler wait for the source computation to complete.

use super::{Continue, Effect};

use std::cell::Cell;
use std::fmt;
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};

use crate::{Effectful, Poll};
use std::pin::Pin;

thread_local! {
    static TLS_CX: Cell<Option<&'static Context>> = Cell::new(None);
}

/// The untyped context of an effectful computation
#[derive(Debug)]
pub struct Context {
    thread: Thread,
}

// raw pointer is just an untyped box, so we can implement Send and Sync (really?)
unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Context {
    /// Create a context within the current thread
    pub fn current() -> Self {
        Context {
            thread: thread::current(),
        }
    }
}

#[derive(Debug)]
pub struct Storage<T> {
    value: Arc<Mutex<Option<T>>>,
}

impl<T> Clone for Storage<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
        }
    }
}

impl<T> Storage<T> {
    fn new() -> Self {
        Storage {
            value: Default::default(),
        }
    }

    pub fn try_take(&self) -> Option<T> {
        self.value.try_lock().ok()?.take()
    }

    pub fn set(&self, v: T) {
        *self.value.lock().unwrap() = Some(v);
    }
}

/// The waker of the task
pub struct Waker<E: Effect> {
    storage: Storage<E::Output>,
    thread: Thread,
}

impl<E: Effect> fmt::Debug for Waker<E>
where
    E::Output: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Waker")
            .field("storage", &self.storage)
            .field("thread", &self.thread)
            .finish()
    }
}

impl<E: Effect> Clone for Waker<E> {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            thread: self.thread.clone(),
        }
    }
}

impl<E: Effect> Waker<E> {
    fn new(storage: Storage<E::Output>, thread: Thread) -> Self {
        Self { storage, thread }
    }

    /// Wake up the task associated with this context
    pub fn wake(&self, v: E::Output) {
        // set handler result
        self.storage.set(v);

        // unpark task thread
        self.thread.unpark()
    }
}

// raw pointer is just an untyped box, so we can implement Send and Sync (really?)
unsafe impl<E> Send for Waker<E>
where
    E: Effect,
    E::Output: Send,
{
}
unsafe impl<E> Sync for Waker<E>
where
    E: Effect,
    E::Output: Sync,
{
}

pub fn channel<E: Effect>(cx: &Context) -> (TypedContext<E>, Storage<E::Output>) {
    let storage = Storage::new();
    let waker = Waker::new(storage.clone(), cx.thread.clone());
    (TypedContext(waker), storage)
}

/// The typed context of an computation
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
    pub fn waker(&self) -> Waker<E> {
        self.0.clone()
    }

    /// Create a `Continue` effect for the completion of the source computation
    pub fn continuation<R>(self) -> Continue<R> {
        Continue::new()
    }

    /// Wake up the task and create a `Continue` effect
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
