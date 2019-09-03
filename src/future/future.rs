use futures::task::waker;

use std::future;
use std::pin::Pin;
use std::sync::Arc;
use std::task;

use crate::Effectful;

/// A future that resolves the underlying computation on `poll`.
/// The computation cannot have any effects to be converted into a future.
pub struct IntoFuture<C>(pub(crate) C);

impl<C> future::Future for IntoFuture<C>
where
    C: Effectful<Effect = !>,
{
    type Output = C::Output;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
        let notify = Arc::new(super::FutureNotify {
            waker: cx.waker().clone(),
        });
        let cx = crate::Context::from_notify(notify);
        let comp = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match comp.poll(&cx) {
            crate::Poll::Done(v) => task::Poll::Ready(v),
            crate::Poll::Pending => task::Poll::Pending,
            crate::Poll::Effect(_) => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct FromFuture<F>(pub(crate) F);

impl<F> Effectful for FromFuture<F>
where
    F: future::Future,
{
    type Output = F::Output;
    type Effect = !;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &crate::Context) -> crate::Poll<Self::Output, Self::Effect> {
        let waker = waker(Arc::new(cx.clone()));
        let mut cx = task::Context::from_waker(&waker);
        let fut = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match fut.poll(&mut cx) {
            task::Poll::Ready(v) => crate::Poll::Done(v),
            task::Poll::Pending => crate::Poll::Pending,
        }
    }
}

pub fn from_future<F>(fut: F) -> FromFuture<F> {
    FromFuture(fut)
}
