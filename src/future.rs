use std::future;
use std::pin::Pin;
use std::sync::Arc;
use std::task;

use super::{Effectful, Notify};

pub struct IntoFuture<C>(pub(crate) C);

impl<C> future::Future for IntoFuture<C>
where
    C: Effectful<Effect = !>,
{
    type Output = C::Output;

    fn poll(self: Pin<&mut Self>, future_cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
        unsafe {
            let notify = Arc::new(FutureNotify {
                waker: future_cx.waker().clone(),
            });
            let effectful_cx = super::Context::from_notify(notify);

            let comp = self.map_unchecked_mut(|this| &mut this.0);
            match comp.poll(&effectful_cx) {
                super::Poll::Done(v) => task::Poll::Ready(v),
                super::Poll::Pending => task::Poll::Pending,
                super::Poll::Effect(_) => unreachable!(),
            }
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
