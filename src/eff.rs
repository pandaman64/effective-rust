use std::any::Any;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

macro_rules! perform {
    ($eff:expr, $ch:expr) => {{
        #[inline(always)]
        fn __getter<T: Effect>(_: &T, channel: Channel) -> impl FnOnce() -> <T as Effect>::Output {
            move || channel.get()
        }
        let getter = __getter(&$eff, $ch.clone());
        yield Into::into($eff);
        getter()
    }};
}

pub trait Effect {
    type Output: Any;
}

pub type ExprWithEffect<E, T> = Box<Generator<Yield = E, Return = T>>;

#[derive(Debug, Default)]
pub struct Channel {
    inner: Rc<RefCell<Option<Box<dyn Any>>>>,
}

impl Channel {
    fn new() -> Self {
        Default::default()
    }

    pub fn set<T: Any>(&self, v: T) {
        *self.inner.borrow_mut() = Some(Box::new(v));
    }

    pub fn get<T: Any>(&self) -> T {
        let value = self.inner.borrow_mut().take().unwrap();
        *value.downcast().unwrap()
    }
}

impl Clone for Channel {
    fn clone(&self) -> Self {
        Channel {
            inner: self.inner.clone(),
        }
    }
}

pub struct Continuation<'h, E, T> {
    expr: ExprWithEffect<E, T>,
    channel: Channel,
    handler: &'h Fn(E, Continuation<E, T>) -> T,
}

impl<'h, E, T> Continuation<'h, E, T> {
    pub fn run<ConcreteEff: Effect>(self, v: ConcreteEff::Output) -> T {
        self.channel.set(v);
        _handle(self.channel, self.expr, self.handler)
    }
}

fn _handle<E, T>(
    channel: Channel,
    mut expr: ExprWithEffect<E, T>,
    handler: &Fn(E, Continuation<E, T>) -> T,
) -> T {
    let state = unsafe { expr.resume() };
    match state {
        GeneratorState::Yielded(effect) => handler(
            effect,
            Continuation {
                expr,
                channel,
                handler,
            },
        ),
        GeneratorState::Complete(v) => v,
    }
}

pub fn handle<E, T, H, G>(gen_func: G, handler: H) -> T
where
    G: FnOnce(Channel) -> ExprWithEffect<E, T>,
    H: Fn(E, Continuation<E, T>) -> T,
{
    let channel = Channel::new();
    let expr = gen_func(channel.clone());
    _handle(channel, expr, &handler)
}
