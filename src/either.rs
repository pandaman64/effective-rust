//! Either of the two underlying effectful computation

use super::{Context, Effectful, Poll};

use std::pin::Pin;

/// An effectful computation that is either of the two underlying ones
pub enum Either<L, R> {
    A(L),
    B(R),
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
        use Either::*;
        unsafe {
            let this = self.get_unchecked_mut();
            match this {
                A(ref mut left) => Pin::new_unchecked(left).poll(cx),
                B(ref mut right) => Pin::new_unchecked(right).poll(cx),
            }
        }
    }
}
