//! Convert a computation with no effects into a future

use futures::task::ArcWake;

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

impl ArcWake for super::Context {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        arc_self.notify().wake();
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
    fn poll(self: Pin<&mut Self>, cx: &super::Context) -> super::Poll<Self::Output, Self::Effect> {
        let waker = Arc::new(cx.clone()).into_waker();
        let mut cx = task::Context::from_waker(&waker);
        let fut = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match fut.poll(&mut cx) {
            task::Poll::Ready(v) => super::Poll::Done(v),
            task::Poll::Pending => super::Poll::Pending,
        }
    }
}

pub fn from_future<F>(fut: F) -> FromFuture<F> {
    FromFuture(fut)
}
