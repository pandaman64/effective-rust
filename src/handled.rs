use super::{coproduct::Either, Context, Continue, Effectful, Poll};

use std::marker::PhantomData;
use std::pin::Pin;

#[derive(Debug)]
pub enum HandlerArgument<T, E> {
    Done(T),
    Effect(E),
}

/// An effectful computation with some effects handled
#[derive(Debug)]
pub struct Handled<C, H, HC, E, I> {
    source: C,
    handler: H,
    handler_stack: Vec<Box<HC>>,
    state: ActiveComputation,
    phantom: PhantomData<(E, I)>,
}

impl<C, H, HC, E, I> Handled<C, H, HC, E, I> {
    pub(crate) fn new(source: C, handler: H) -> Self {
        Handled {
            source,
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
    H: FnMut(HandlerArgument<Output, HandledEffect>) -> HC,
    HC: Effectful<Output = NewOutput, Effect = Either<Continue<Output>, NewEffect>>,
    Effect: super::coproduct::Subset<HandledEffect, I, Remainder = NewEffect>,
{
    type Output = NewOutput;
    type Effect = NewEffect;

    // I'm not sure whether this inline improves the performance;
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
                        match Pin::new_unchecked(&mut this.source).poll(cx) {
                            Done(v) => {
                                this.state = ActiveComputation::Handler;
                                if this.handler_stack.is_empty() {
                                    let comp = (this.handler)(HandlerArgument::Done(v));
                                    this.handler_stack.push(Box::new(comp));
                                } else {
                                    // fulfill Continue<Output> effect
                                    cx.set(v);
                                }
                            }
                            Effect(e) => match e.subset() {
                                Ok(e) => {
                                    this.state = ActiveComputation::Handler;
                                    let comp = (this.handler)(HandlerArgument::Effect(e));
                                    this.handler_stack.push(Box::new(comp));
                                }
                                Err(rem) => return Effect(rem),
                            },
                            NotReady => return NotReady,
                        }
                    }
                    ActiveComputation::Handler => {
                        let handler = this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).poll(cx) {
                            Done(v) => {
                                this.handler_stack.pop();

                                // the last handler
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    cx.set(v);
                                }
                            }
                            Effect(Either::A(_, _)) => {
                                // continue the original computation
                                this.state = ActiveComputation::Source;

                                // if the handler has already waken the task, continue the computation
                                // otherwise, wait until wake() is called
                                if !cx.contains() {
                                    return NotReady;
                                }
                            }
                            Effect(Either::B(e)) => return Effect(e),
                            NotReady => return NotReady,
                        }
                    }
                }
            }
        }
    }
}
