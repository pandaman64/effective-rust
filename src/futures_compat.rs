//! Conversion between Future/Stream and Effectful

use futures::task::ArcWake;

use std::sync::Arc;
use std::task;

use super::Notify;

pub mod future;
pub mod stream;

struct FutureNotify {
    waker: task::Waker,
}

impl Notify for FutureNotify {
    fn wake(&self) {
        self.waker.wake_by_ref();
    }
}

impl ArcWake for super::Context {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        arc_self.notify().wake();
    }
}
