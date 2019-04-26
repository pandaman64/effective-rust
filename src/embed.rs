use super::{coproduct::Embed, Context, Effectful, Poll};

use std::marker::PhantomData;
use std::pin::Pin;

/// An effectful computation created by `embed()` combinator
///
/// `EmbedEffect` is used to "widen" the effect that the task will perform
pub struct EmbedEffect<C, Target, Indices>(C, PhantomData<(Target, Indices)>);

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

        match unsafe { self.map_unchecked_mut(|this| &mut this.0) }.poll(cx) {
            Done(v) => Done(v),
            Effect(e) => Effect(e.embed()),
            NotReady => NotReady,
        }
    }
}
