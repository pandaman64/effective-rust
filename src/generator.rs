//! Convert a generator into an effectful computation

use super::{context::set_task_context, Context, Effectful, Event, Poll};

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use pin_project::pin_project;

#[pin_project]
struct GenEffectful<G>(#[pin] G);

/// Create an effectful computation which wraps a generator
pub fn from_generator<G, Output, Effect>(x: G) -> impl Effectful<Output = Output, Effect = Effect>
where
    G: Generator<Return = Output, Yield = Poll<!, Effect>>,
{
    GenEffectful(x)
}

impl<G, Output, Effect> Effectful for GenEffectful<G>
where
    G: Generator<Return = Output, Yield = Poll<!, Effect>>,
{
    type Output = Output;
    type Effect = Effect;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        set_task_context(cx, || match self.project().0.resume() {
            GeneratorState::Complete(v) => Poll::complete(v),
            GeneratorState::Yielded(Poll::Event(Event::Effect(e))) => Poll::effect(e),
            GeneratorState::Yielded(Poll::Pending) => Poll::Pending,
            GeneratorState::Yielded(Poll::Event(Event::Complete(never))) => never,
        })
    }
}
