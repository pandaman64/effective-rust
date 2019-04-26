use super::{Context, Effectful, Poll};

use std::pin::Pin;

/// A boxed effectful computation with type erased
pub struct Boxed<'a, Output, Effect>(Box<dyn Effectful<Output = Output, Effect = Effect> + 'a>);

impl<'a, Output, Effect> Boxed<'a, Output, Effect> {
    pub(crate) fn new<C: Effectful<Output = Output, Effect = Effect> + 'a>(source: C) -> Self {
        Self(Box::new(source))
    }
}

impl<'a, Output, Effect> Effectful for Boxed<'a, Output, Effect> {
    type Output = Output;
    type Effect = Effect;

    /// Poll the inner computation
    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        use std::ops::DerefMut;
        unsafe {
            let this = self.get_unchecked_mut();
            let r = this.0.deref_mut();
            Pin::new_unchecked(r).poll(cx)
        }
    }
}
