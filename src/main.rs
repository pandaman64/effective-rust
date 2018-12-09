#![feature(fnbox, nll, generators, generator_trait)]
#![feature(get_type_id, core_intrinsics)]

use std::any::Any;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

#[derive(Debug, Default)]
struct Channel {
    inner: Rc<RefCell<Option<Box<dyn Any>>>>,
}

impl Channel {
    fn new() -> Self {
        Default::default()
    }

    fn set<T: Any + Debug>(&self, v: T) {
        println!("setting {:?} of type {}", v, unsafe {
            std::intrinsics::type_name::<T>()
        });
        *self.inner.borrow_mut() = Some(Box::new(v));
    }

    fn get<T: Any + Debug>(&self) -> T {
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

struct Continuation<'h, T> {
    expr: ExprWithEffect<T>,
    channel: Channel,
    handler: &'h Fn(Effect, Continuation<T>) -> T,
}

impl<'h, T> Continuation<'h, T> {
    fn run<V: 'static + Any + Debug>(self, v: V) -> T {
        self.channel.set(v);
        _handle(self.channel, self.expr, self.handler)
    }
}

#[derive(Debug)]
enum Effect {
    Foo,
    Bar,
}

type ExprWithEffect<T> = Box<Generator<Yield = Effect, Return = T>>;

fn expr_with_effect(channel: Channel) -> ExprWithEffect<u32> {
    Box::new(move || {
        let v1 =
        // perform Effect::Foo
        {
            yield Effect::Foo;
            channel.get::<u32>()
        };
        let v2 =
        // perform Effect::Bar
        {
            yield Effect::Bar;
            channel.get::<u32>()
        };
        v1 * v2
    })
}

fn _handle<T>(
    channel: Channel,
    mut expr: ExprWithEffect<T>,
    handler: &Fn(Effect, Continuation<T>) -> T,
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

fn handle<T, H, G>(gen_func: G, handler: H) -> T
where
    G: FnOnce(Channel) -> ExprWithEffect<T>,
    H: Fn(Effect, Continuation<T>) -> T,
{
    let channel = Channel::default();
    let expr = gen_func(channel.clone());
    _handle(channel, expr, &handler)
}

fn main() {
    let result = handle(expr_with_effect, |eff, k| match eff {
        Effect::Foo => {
            println!("foo");
            k.run(13_u32)
        }
        Effect::Bar => {
            println!("bar");
            k.run(4_u32)
        }
    });

    println!("{}", result);
}
