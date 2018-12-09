use std::any::Any;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

macro_rules! perform {
    ($eff:expr, $ch:expr) => {{
        yield $eff;
        $ch.get()
    }};
    ($eff:expr, $ch:expr, $ty:ty) => {{
        yield $eff;
        $ch.get::<$ty>()
    }};
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

    pub fn set<T: Any + Debug>(&self, v: T) {
        println!("setting {:?} of type {}", v, unsafe {
            std::intrinsics::type_name::<T>()
        });
        *self.inner.borrow_mut() = Some(Box::new(v));
    }

    pub fn get<T: Any + Debug>(&self) -> T {
        let value = self.inner.borrow_mut().take().unwrap();
        println!("getting of type {}", unsafe {
            std::intrinsics::type_name::<T>()
        });
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
    pub fn run<V: 'static + Any + Debug>(self, v: V) -> T {
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
