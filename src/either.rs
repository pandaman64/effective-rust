//! Either of the two underlying effectful computation

use super::{Context, Effectful, Poll};

use std::pin::Pin;

use pin_project::{pin_project, project};

/// An effectful computation that is either of the two underlying ones
#[pin_project]
pub enum Either<L, R> {
    A(#[pin] L),
    B(#[pin] R),
}

impl<Output, Effect, L, R> Effectful for Either<L, R>
where
    L: Effectful<Output = Output, Effect = Effect>,
    R: Effectful<Output = Output, Effect = Effect>,
{
    type Output = Output;
    type Effect = Effect;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &Context) -> Poll<Self::Output, Self::Effect> {
        #[project]
        match self.project() {
            Either::A(left) => left.poll(cx),
            Either::B(right) => right.poll(cx),
        }
    }
}
