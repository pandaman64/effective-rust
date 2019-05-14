//! A context for a task
//!
//! [TypedContext](struct.TypedContext.html) corresponds to a continuation. You can wake the task
//! via `.waker()` and perform a special effect called [Continue](../struct.Continue.html).
//! `Continue` effect makes the handler wait for the source computation to complete.

use super::{Continue, Effect};

use std::cell::Cell;
use std::fmt;
use std::intrinsics::type_name;
use std::ptr::NonNull;
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};

use crate::{Effectful, Poll};
use log::debug;
use std::pin::Pin;

thread_local! {
    static TLS_CX: Cell<Option<&'static Context>> = Cell::new(None);
}

extern "Rust" {
    type Whatever;
}

/// The untyped context of an effectful computation
#[derive(Debug)]
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

    /// Returns true if the task-local storage contains any value
    ///
    /// Returns false if it does not.
    pub fn contains(&self) -> bool {
        self.storage.lock().unwrap().is_some()
    }

    /// Takes the value out of the task-local storage
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

    /// Assign a value to the task-local storage
    pub fn set<T>(&self, v: T) {
        unsafe {
            debug!("Context::set: {}", type_name::<T>());
            *self.storage.lock().unwrap() = Some(NonNull::new_unchecked(
                Box::into_raw(Box::new(v)) as *mut Whatever,
            ));
        }
    }

    /// Create a typed context which shares the same task-local storage
    pub fn typed<E: Effect>(&self) -> TypedContext<E> {
        let ptr =
            Arc::into_raw(Arc::clone(&self.storage)) as *const Mutex<Option<NonNull<E::Output>>>;
        unsafe {
            TypedContext(Waker {
                storage: Arc::from_raw(ptr),
                thread: self.thread.clone(),
            })
        }
    }
}

/// The waker of the task
pub struct Waker<E: Effect> {
    storage: Arc<Mutex<Option<NonNull<E::Output>>>>,
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
            storage: Arc::clone(&self.storage),
            thread: self.thread.clone(),
        }
    }
}

impl<E: Effect> Waker<E> {
    /// Wake up the task associated with this context
    pub fn wake(&self, v: E::Output) {
        // set handler result
        unsafe {
            debug!(
                "Waker::set: {} -> {}, to: {:?}",
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

/// Helper function for taking the output of an effect of the desired type out of the task-local storage
pub fn gen_taker<E>(_e: &E) -> fn(&Context) -> Option<E::Output>
where
    E: Effect,
{
    Context::take::<E::Output>
}

struct SetOnDrop(Option<&'static Context>);

impl Drop for SetOnDrop {
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
    let _reset = SetOnDrop(old_cx);
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
