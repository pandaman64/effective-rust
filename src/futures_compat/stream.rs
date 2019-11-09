use futures::stream::Stream;
use futures::task::waker;
use pin_project::pin_project;

use std::pin::Pin;
use std::sync::Arc;
use std::task;

use crate::{Effect, Effectful, Event};

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

#[pin_project]
#[derive(Debug)]
pub struct IntoStream<C>(#[pin] C);

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
        let comp = self.project().0;
        match comp.poll(&cx) {
            crate::Poll::Event(Event::Complete(())) => task::Poll::Ready(None),
            crate::Poll::Event(Event::Effect(e)) => task::Poll::Ready(Some(e)),
            crate::Poll::Pending => task::Poll::Pending,
        }
    }
}

impl<T> Effect for Item<T> {
    type Output = ();
}

#[pin_project]
#[derive(Debug)]
pub struct FromStream<S>(#[pin] S);

impl<S> Effectful for FromStream<S>
where
    S: Stream,
{
    type Output = ();
    type Effect = Item<S::Item>;

    fn poll(self: Pin<&mut Self>, cx: &crate::Context) -> crate::Poll<Self::Output, Self::Effect> {
        let waker = waker(Arc::new(cx.clone()));
        let mut cx = task::Context::from_waker(&waker);
        let stream = self.project().0;
        match stream.poll_next(&mut cx) {
            task::Poll::Ready(Some(v)) => crate::Poll::effect(Item(v)),
            task::Poll::Ready(None) => crate::Poll::complete(()),
            task::Poll::Pending => crate::Poll::Pending,
        }
    }
}

pub fn from_stream<S>(s: S) -> FromStream<S> {
    FromStream(s)
}
