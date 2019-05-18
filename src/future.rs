//! Convert a computation with no effects into a future

use std::future;
use std::pin::Pin;
use std::sync::Arc;
use std::task;

use super::{Effectful, Notify};

/// A future that resolves the underlying computation on `poll`.
/// The computation cannot have any effects to be converted into a future.
pub struct IntoFuture<C>(pub(crate) C);

impl<C> future::Future for IntoFuture<C>
where
    C: Effectful<Effect = !>,
{
    type Output = C::Output;

    #[inline]
    fn poll(self: Pin<&mut Self>, future_cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
        let notify = Arc::new(FutureNotify {
            waker: future_cx.waker().clone(),
        });
        let effectful_cx = super::Context::from_notify(notify);

        let comp = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match comp.poll(&effectful_cx) {
            super::Poll::Done(v) => task::Poll::Ready(v),
            super::Poll::Pending => task::Poll::Pending,
            super::Poll::Effect(_) => unreachable!(),
        }
    }
}

struct FutureNotify {
    waker: task::Waker,
}

impl Notify for FutureNotify {
    fn wake(&self) {
        self.waker.wake_by_ref();
    }
}
