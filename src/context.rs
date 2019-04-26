use super::{Continue, Effect};

use std::cell::Cell;
use std::intrinsics::type_name;
use std::ptr::NonNull;
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};

use log::debug;

thread_local! {
    static TLS_CX: Cell<Option<Context>> = Cell::new(None);
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
///
/// # Panics
/// Panics if the thread-local task is not set
pub fn get_task_context() -> Context {
    TLS_CX
        .with(|tls_cx| {
            let cx = tls_cx.replace(None).take();
            tls_cx.set(cx.clone());
            cx
        })
        .expect("thread local context must be set")
}
