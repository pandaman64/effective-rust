#![feature(fnbox, nll, generators, generator_trait)]
#![feature(trace_macros)]

use std::any::Any;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

// https://users.rust-lang.org/t/macro-to-replace-type-parameters/17903
#[macro_export]
macro_rules! eff {
    // Open parenthesis.
    (@$ctx:ident, @($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        eff!(@$ctx, @(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    (@$ctx:ident, @($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        eff!(@$ctx, @(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    (@$ctx:ident, @($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        eff!(@$ctx, @(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        eff!(@$ctx, @(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        eff!(@$ctx, @(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        eff!(@$ctx, @(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `perform!($e)` tokens with `perform_impl!(@$ctx, $e)`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) perform!($e:expr) $($rest:tt)*) => {
        eff!(@$ctx, @(($($top)* perform_impl!(@$ctx, $e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `perform!`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        eff!(@$ctx, @(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    (@$ctx:ident, @(($($top:tt)+))) => {{
        $($top)+
    }};

    // Begin with an empty stack.
    ($($input:tt)+) => {{
        |context: Context<_, _>| -> WithEffect<_, _> {
            WithEffect {
                inner: Box::new(move || { eff!(@context, @(()) $($input)*) })
            }
        }
    }};
}

#[macro_export]
macro_rules! perform_impl {
    (@$ch:ident, $eff:expr) => {{
        #[inline(always)]
        fn __getter<T: Effect>(_: &T, channel: Channel) -> impl FnOnce() -> <T as Effect>::Output {
            move || channel.get()
        }
        let eff = $eff;
        let getter = __getter(&eff, $ch.channel.clone());
        yield Into::into(eff);
        getter()
    }};
}

pub trait Effect {
    type Output: Any;
}

pub struct WithEffect<E, T> {
    pub inner: Box<Generator<Yield = E, Return = T>>,
}

impl<E, T> WithEffect<E, T> {
    pub fn invoke(self, context: Context<E, T>) -> T {
        _handle(context, self)
    }
}

#[derive(Debug, Default)]
pub struct Channel {
    pub inner: Rc<RefCell<Option<Box<dyn Any>>>>,
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

pub struct Context<E, T> {
    pub channel: Channel,
    pub handler: Rc<Fn(E, Continuation<E, T>) -> T>,
}

impl<E, T> Clone for Context<E, T> {
    fn clone(&self) -> Self {
        Context {
            channel: self.channel.clone(),
            handler: self.handler.clone(),
        }
    }
}

pub struct Continuation<E, T> {
    expr: WithEffect<E, T>,
    context: Context<E, T>,
}

impl<E, T> Continuation<E, T> {
    pub fn run<ConcreteEff: Effect>(self, v: ConcreteEff::Output) -> T {
        self.context.channel.set(v);
        _handle(self.context, self.expr)
    }
}

fn _handle<E, T>(context: Context<E, T>, mut expr: WithEffect<E, T>) -> T {
    let state = unsafe { expr.inner.resume() };
    match state {
        GeneratorState::Yielded(effect) => {
            let handler = context.handler.clone();
            handler(effect, Continuation { expr, context })
        }
        GeneratorState::Complete(v) => v,
    }
}

pub fn handle<E, T, H, G, VH, R>(gen_func: G, value_handler: VH, handler: H) -> R
where
    G: FnOnce(Context<E, T>) -> WithEffect<E, T>,
    H: Fn(E, Continuation<E, T>) -> T + 'static,
    VH: FnOnce(T) -> R,
{
    let context = Context {
        channel: Channel::new(),
        handler: Rc::new(handler),
    };
    let expr = gen_func(context.clone());
    value_handler(_handle(context, expr))
}

#[macro_export]
macro_rules! handler {
    ( $($variant:ident @ $eff_type:ty [ $eff:pat, $k:pat ] => $e:expr),* ) => {{
        enum Effects {
            $($variant($eff_type),)*
        }

        $(
            impl From<$eff_type> for Effects {
                fn from(v: $eff_type) -> Self {
                    Effects::$variant(v)
                }
            }
        )*

        |eff: Effects, k| match eff {
            $(
                Effects::$variant($eff) => {
                    let $k = k;
                    $e
                }
            )*
        }
    }};
}
