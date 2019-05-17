//! An effectful computation with some effects handled

use super::{coproduct::Either, AwaitedPoll, Context, Continue, Effectful, Poll, Waker};

use std::fmt;
use std::marker::PhantomData;
use std::pin::Pin;

struct Handler<HC: Effectful> {
    computation: Pin<Box<HC>>,
    waker: Option<Waker<Continue<HC::Output>>>,
}

impl<HC> fmt::Debug for Handler<HC>
where
    HC: Effectful + fmt::Debug,
    HC::Output: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Handler")
            .field("computation", &self.computation)
            .field("waker", &self.waker)
            .finish()
    }
}

impl<HC: Effectful> Handler<HC> {
    fn new(computation: HC) -> Self {
        Self {
            computation: Box::pin(computation),
            waker: None,
        }
    }
}

/// An effectful computation with some effects handled
pub struct Handled<C, H, HC, E, I>
where
    HC: Effectful,
{
    source: Option<C>,
    handler: H,
    handler_stack: Vec<Handler<HC>>,
    state: ActiveComputation,
    phantom: PhantomData<(E, I)>,
}

impl<C, H, HC, E, I> fmt::Debug for Handled<C, H, HC, E, I>
where
    C: fmt::Debug,
    H: fmt::Debug,
    HC: Effectful + fmt::Debug,
    HC::Output: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Handled")
            .field("source", &self.source)
            .field("handler", &self.handler)
            .field("handler stack", &self.handler_stack)
            .field("state", &self.state)
            .finish()
    }
}

impl<C, H, HC: Effectful, E, I> Handled<C, H, HC, E, I> {
    pub(crate) fn new(source: C, handler: H) -> Self {
        Handled {
            source: Some(source),
            handler,
            handler_stack: vec![],
            state: ActiveComputation::Source,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug)]
enum ActiveComputation {
    Source,
    Handler,
}

impl<C, Output, Effect, H, HC, HandledEffect, NewOutput, NewEffect, I> Effectful
    for Handled<C, H, HC, HandledEffect, I>
where
    C: Effectful<Output = Output, Effect = Effect>,
    H: FnMut(AwaitedPoll<Output, HandledEffect>) -> HC,
    HC: Effectful<Output = NewOutput, Effect = Either<Continue<NewOutput>, NewEffect>>,
    Effect: super::coproduct::Subset<HandledEffect, I, Remainder = NewEffect>,
{
    type Output = NewOutput;
    type Effect = NewEffect;

    // I'm not sure if this inline improves performance;
    // this method is much larger than I expected
    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        use Poll::*;

        // TODO: verify soundness
        unsafe {
            let this = self.as_mut().get_unchecked_mut();
            loop {
                match &mut this.state {
                    ActiveComputation::Source => {
                        match Pin::new_unchecked(
                            this.source.as_mut().expect("poll after completion"),
                        )
                        .poll(cx)
                        {
                            Done(v) => {
                                this.source = None;
                                this.state = ActiveComputation::Handler;

                                // TODO: what if this.handler panics?
                                let comp = (this.handler)(AwaitedPoll::Done(v));
                                this.handler_stack.push(Handler::new(comp));
                            }
                            Effect(e) => match e.subset() {
                                Ok(e) => {
                                    this.state = ActiveComputation::Handler;
                                    // TODO: what if this.handler panics?
                                    let comp = (this.handler)(AwaitedPoll::Effect(e));
                                    this.handler_stack.push(Handler::new(comp));
                                }
                                Err(rem) => return Effect(rem),
                            },
                            Pending => return Pending,
                        }
                    }
                    ActiveComputation::Handler => {
                        let handler = &mut this.handler_stack.last_mut().unwrap().computation;
                        match handler.as_mut().poll(cx) {
                            Done(v) => {
                                this.handler_stack.pop();

                                // the last handler
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    (this.handler_stack.last_mut().unwrap().waker)
                                        .take()
                                        .unwrap()
                                        .wake(v);
                                }
                            }
                            Effect(Either::A(_, cx)) => {
                                // continue the original computation
                                this.state = ActiveComputation::Source;

                                this.handler_stack.last_mut().unwrap().waker = Some(cx.waker());
                            }
                            Effect(Either::B(e)) => return Effect(e),
                            Pending => return Pending,
                        }
                    }
                }
            }
        }
    }
}
