use super::{Context, Effectful, Poll};

use std::pin::Pin;

#[derive(Debug)]
pub struct PollFn<F>(F);

// Safe because we will not create Pin<&mut F>
impl<F> Unpin for PollFn<F> {}

impl<T, Effect, F> Effectful for PollFn<F>
where
    F: FnMut(&Context) -> Poll<T, Effect>,
{
    type Output = T;
    type Effect = Effect;

    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        (&mut self.0)(cx)
    }
}

pub fn poll_fn<T, Effect, F: FnMut(&Context) -> Poll<T, Effect>>(f: F) -> PollFn<F> {
    PollFn(f)
}
