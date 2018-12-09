use std::any::Any;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

// https://users.rust-lang.org/t/macro-to-replace-type-parameters/17903
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
    ($($input:tt)+) => {
        |channel: Channel| {
            Box::new(move || { eff!(@channel, @(()) $($input)*) }) as ExprWithEffect<_, _>
        }
    };
}

macro_rules! perform_impl {
    (@$ch:ident, $eff:expr) => {{
        #[inline(always)]
        fn __getter<T: Effect>(_: &T, channel: Channel) -> impl FnOnce() -> <T as Effect>::Output {
            move || channel.get()
        }
        let eff = $eff;
        let getter = __getter(&eff, $ch.clone());
        yield Into::into(eff);
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
