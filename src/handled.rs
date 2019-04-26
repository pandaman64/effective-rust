use super::{coproduct::Either, Context, Continue, Effectful, Poll};

use std::pin::Pin;

/// An effectful computation with some effects handled
pub struct Handled<C, H, HC, VH, VHC> {
    source: C,
    value_handler: Option<VH>,
    handler: H,
    handler_stack: Vec<Box<HC>>,
    state: ActiveComputation<VHC>,
}

impl<C, H, HC, VH, VHC> Handled<C, H, HC, VH, VHC> {
    pub(crate) fn new(source: C, value_handler: VH, handler: H) -> Self {
        Handled {
            source,
            value_handler: Some(value_handler),
            handler,
            handler_stack: vec![],
            state: ActiveComputation::Source,
        }
    }
}

enum ActiveComputation<VHC> {
    Source,
    Handler,
    ValueHandler(VHC),
}

impl<C, Output, Effect, H, HC, VH, VHC, NewOutput, NewEffect> Effectful
    for Handled<C, H, HC, VH, VHC>
where
    C: Effectful<Output = Output, Effect = Effect>,
    VH: FnOnce(Output) -> VHC,
    H: FnMut(Effect) -> Result<HC, NewEffect>,
    VHC: Effectful<Output = NewOutput, Effect = NewEffect>,
    HC: Effectful<Output = NewOutput, Effect = Either<Continue<Output>, NewEffect>>,
{
    type Output = NewOutput;
    type Effect = NewEffect;

    // I'm not sure whether this inline improves the performance;
    // this method is much larger than I expected
    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: Context) -> Poll<Self::Output, Self::Effect> {
        use Poll::*;

        // TODO: verify soundness
        unsafe {
            let this = self.as_mut().get_unchecked_mut();
            loop {
                match &mut this.state {
                    ActiveComputation::Source => {
                        match Pin::new_unchecked(&mut this.source).poll(cx.clone()) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    this.state = ActiveComputation::ValueHandler(this
                                        .value_handler
                                        .take()
                                        .expect("poll after completion")(
                                        v
                                    ));
                                } else {
                                    // fulfill Continue<Output> effect
                                    this.state = ActiveComputation::Handler;
                                    cx.set(v);
                                }
                            }
                            Effect(e) => match (this.handler)(e) {
                                Ok(x) => {
                                    this.handler_stack.push(Box::new(x));
                                    this.state = ActiveComputation::Handler;
                                }
                                Err(e) => return Effect(e),
                            },
                            NotReady => return NotReady,
                        }
                    }
                    ActiveComputation::Handler => {
                        let handler = this.handler_stack.last_mut().unwrap();
                        match Pin::new_unchecked(&mut **handler).poll(cx.clone()) {
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
                    ActiveComputation::ValueHandler(ref mut x) => {
                        match Pin::new_unchecked(x).poll(cx.clone()) {
                            Done(v) => {
                                if this.handler_stack.is_empty() {
                                    return Done(v);
                                } else {
                                    cx.set(v);
                                    this.state = ActiveComputation::Handler;
                                }
                            }
                            Effect(e) => return Effect(e),
                            NotReady => return NotReady,
                        }
                    }
                }
            }
        }
    }
}
