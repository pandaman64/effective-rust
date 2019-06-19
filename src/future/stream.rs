use futures::stream::Stream;
use futures::task::ArcWake;

use std::pin::Pin;
use std::sync::Arc;
use std::task;

use crate::{Effect, Effectful};

#[derive(Debug)]
pub struct Item<T>(T);

impl<T> Item<T> {
    pub fn new(v: T) -> Self {
        Self(v)
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

#[derive(Debug)]
pub struct IntoStream<C>(C);

impl<C> Stream for IntoStream<C>
where
    C: Effectful<Output = ()>,
{
    type Item = C::Effect;

    fn poll_next(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<Option<Self::Item>> {
        let notify = Arc::new(super::FutureNotify {
            waker: cx.waker().clone(),
        });
        let cx = crate::Context::from_notify(notify);
        let comp = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match comp.poll(&cx) {
            crate::Poll::Done(()) => task::Poll::Ready(None),
            crate::Poll::Effect(e) => task::Poll::Ready(Some(e)),
            crate::Poll::Pending => task::Poll::Pending,
        }
    }
}

impl<T> Effect for Item<T> {
    type Output = ();
}

#[derive(Debug)]
pub struct FromStream<S>(S);

impl<S> Effectful for FromStream<S>
where
    S: Stream,
{
    type Output = ();
    type Effect = Item<S::Item>;

    fn poll(self: Pin<&mut Self>, cx: &crate::Context) -> crate::Poll<Self::Output, Self::Effect> {
        let waker = Arc::new(cx.clone()).into_waker();
        let mut cx = task::Context::from_waker(&waker);
        let stream = unsafe { self.map_unchecked_mut(|this| &mut this.0) };
        match stream.poll_next(&mut cx) {
            task::Poll::Ready(Some(v)) => crate::Poll::Effect(Item(v)),
            task::Poll::Ready(None) => crate::Poll::Done(()),
            task::Poll::Pending => crate::Poll::Pending,
        }
    }
}

pub fn from_stream<S>(s: S) -> FromStream<S> {
    FromStream(s)
}
