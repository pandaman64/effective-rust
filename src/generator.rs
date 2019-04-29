//! Convert a generator into an effectful computation

use super::{context::set_task_context, Context, Effectful, Poll, Suspension};

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

struct GenEffectful<G>(G);

/// Create an effectful computation which wraps a generator
pub fn from_generator<G, Output, Effect>(x: G) -> impl Effectful<Output = Output, Effect = Effect>
where
    G: Generator<Return = Output, Yield = Suspension<Effect>>,
{
    GenEffectful(x)
}

impl<G, Output, Effect> Effectful for GenEffectful<G>
where
    G: Generator<Return = Output, Yield = Suspension<Effect>>,
{
    type Output = Output;
    type Effect = Effect;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        use GeneratorState::*;
        use Poll::*;

        unsafe {
            let this = &mut self.get_unchecked_mut().0;
            set_task_context(cx, || match Pin::new_unchecked(this).resume() {
                Complete(v) => Done(v),
                Yielded(Suspension::Effect(e)) => Effect(e),
                Yielded(Suspension::Pending) => Pending,
            })
        }
    }
}
