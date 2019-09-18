//! Embed the effect of an computation into a wider one

use pin_project::pin_project;

use super::{coproduct::Embed, Context, Effectful, Poll};

use std::marker::PhantomData;
use std::pin::Pin;

/// An effectful computation created by `embed()` combinator
///
/// `EmbedEffect` is used to "widen" the effect that the task will perform
#[pin_project]
pub struct EmbedEffect<C, Target, Indices>(#[pin] C, PhantomData<(Target, Indices)>);

impl<C, Target, Indices> EmbedEffect<C, Target, Indices> {
    pub(crate) fn new(source: C) -> Self {
        Self(source, PhantomData)
    }
}

impl<C, Target, Indices> Effectful for EmbedEffect<C, Target, Indices>
where
    C: Effectful,
    C::Effect: Embed<Target, Indices>,
{
    type Output = C::Output;
    type Effect = Target;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        use Poll::*;

        match self.project_into().0.poll(cx) {
            Done(v) => Done(v),
            Effect(e) => Effect(e.embed()),
            Pending => Pending,
        }
    }
}
