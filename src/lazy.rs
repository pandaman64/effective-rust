use super::{Context, Effectful, Poll};

use std::pin::Pin;

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
    /// Panics if the task is polled again after completion
    #[inline]
    fn poll(self: Pin<&mut Self>, _cx: &Context) -> Poll<Self::Output, Self::Effect> {
        unsafe {
            let this = self.get_unchecked_mut();
            Poll::Done(this.0.take().expect("poll after completion")())
        }
    }
}

/// Converts a thunk into a one-shot effectful computation
/// This function is usually used to convert closures into an effect handler
/// (which must be effectful)
#[inline]
pub fn lazy<T, F>(v: F) -> impl Effectful<Output = T, Effect = !>
where
    F: FnOnce() -> T,
{
    Lazy(Some(v))
}

/// Converts a value into a one-shot effectful computation that immediately resolves to said value
pub fn pure<T>(v: T) -> impl Effectful<Output = T, Effect = !> {
    lazy(move || v)
}
