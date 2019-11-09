use futures::task::waker;
use pin_project::pin_project;

use std::future;
use std::pin::Pin;
use std::sync::Arc;
use std::task;

use crate::{Effectful, Event};

/// A future that resolves the underlying computation on `poll`.
/// The computation cannot have any effects to be converted into a future.
#[pin_project]
pub struct IntoFuture<C>(#[pin] pub(crate) C);

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
        let comp = self.project().0;
        match comp.poll(&cx) {
            crate::Poll::Event(Event::Complete(v)) => task::Poll::Ready(v),
            crate::Poll::Pending => task::Poll::Pending,
            crate::Poll::Event(Event::Effect(never)) => never,
        }
    }
}

#[pin_project]
#[derive(Debug)]
pub struct FromFuture<F>(#[pin] pub(crate) F);

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
        let fut = self.project().0;
        match fut.poll(&mut cx) {
            task::Poll::Ready(v) => crate::Poll::complete(v),
            task::Poll::Pending => crate::Poll::Pending,
        }
    }
}

pub fn from_future<F>(fut: F) -> FromFuture<F> {
    FromFuture(fut)
}
