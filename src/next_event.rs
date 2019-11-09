use crate::{Context, Effectful, Event, Poll};
use std::pin::Pin;

/// A combinator created by `next_event`.
///
/// This struct polls the given computation until the first event occurs.
/// It's valid to poll again after this computation completes.
pub enum NextEvent<C: Effectful> {
    /// the computation has not generated an event yet
    Computation(C),
    /// An event occured
    Event(Event<C::Output, C::Effect>),
    /// The event has been taken by `take_event`
    Gone,
}

impl<C: Effectful> NextEvent<C> {
    /// Checks if the underlying computation generated an event
    pub fn occured(&self) -> bool {
        match self {
            NextEvent::Computation(_) => false,
            _ => true,
        }
    }

    /// Take the event if occured
    pub fn take_event(self: Pin<&mut Self>) -> Option<Event<C::Output, C::Effect>> {
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                NextEvent::Event(_) => {
                    let ret = match std::mem::replace(this, NextEvent::Gone) {
                        NextEvent::Event(ev) => ev,
                        _ => unreachable!(),
                    };
                    Some(ret)
                }
                _ => None,
            }
        }
    }
}

impl<C: Effectful> Effectful for NextEvent<C> {
    type Output = ();
    type Effect = !;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                NextEvent::Computation(ref mut c) => match Pin::new_unchecked(c).poll(cx) {
                    Poll::Event(ev) => {
                        *this = NextEvent::Event(ev);
                        Poll::complete(())
                    }
                    Poll::Pending => Poll::Pending,
                },
                _ => Poll::complete(()),
            }
        }
    }
}
