#![feature(fnbox, nll, generators, generator_trait)]
#![feature(trace_macros)]

use std::boxed::FnBox;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

// https://users.rust-lang.org/t/macro-to-replace-type-parameters/17903
#[macro_export]
macro_rules! eff_muncher {
    // Open parenthesis.
    (@$ctx:ident, @($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    (@$ctx:ident, @($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    (@$ctx:ident, @($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    (@$ctx:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `perform!($e)` tokens with `perform_impl!(@$ctx, $e)`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) perform!($e:expr) $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* perform_impl!(@$ctx, $e)) $($stack)*) $($rest)*)
    };

    // Replace `invoke!($e)` tokens with `invoke_impl!(@$ctx, $e)`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) invoke!($e:expr) $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* invoke_impl!(@$ctx, $e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `perform!` nor `invoke!`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    (@$ctx:ident, @(($($top:tt)+))) => {{
        $($top)+
    }};
}

#[macro_export]
macro_rules! eff {
    // Begin with an empty stack.
    ($($input:tt)+) => {{
        Box::new(|context: Context<_, _, _>| -> WithEffectInner<_, _> {
            WithEffectInner {
                inner: Box::new(
                    #[allow(unreachable_code)]
                    move || {
                        // This trick lets the compiler treat this closure
                        // as a generator even if $input doesn't contain no perform
                        // (no yield).
                        // see: https://stackoverflow.com/a/53757228/8554666
                        if false { yield unreachable!(); }
                        eff_muncher!(@context, @(()) $($input)*)
                    }
                )
            }
        })
    }};
}

#[macro_export]
macro_rules! perform_impl {
    (@$ctx:ident, $eff:expr) => {{
        #[inline(always)]
        fn __getter<E: Effect, C: Perform<E>>(
            _: &E,
            channel: Channel<C>,
        ) -> impl FnOnce() -> <E as Effect>::Output {
            move || channel.get::<E>()
        }
        let eff = $eff;
        let getter = __getter(&eff, $ctx.channel.clone());
        yield Into::into(eff);
        getter()
    }};
}

#[macro_export]
macro_rules! invoke_impl {
    (@$ctx:ident, $eff:expr) => {{
        $eff($ctx.clone()).invoke($ctx.clone())
    }};
}

pub type WithEffect<E, T, C> = Box<FnBox(Context<E, T, C>) -> WithEffectInner<E, T>>;

pub trait Effect {
    type Output;
}

pub struct WithEffectInner<E, T> {
    pub inner: Box<Generator<Yield = E, Return = T>>,
}

impl<E, T> WithEffectInner<E, T> {
    pub fn invoke<C>(self, context: Context<E, T, C>) -> T {
        _handle(context, self)
    }
}

pub trait Perform<E>
where
    E: Effect,
{
    fn into(self) -> E::Output;
}

#[derive(Debug)]
pub struct Channel<C> {
    pub inner: Rc<RefCell<Option<C>>>,
}

impl<C> Channel<C> {
    fn new() -> Self {
        Default::default()
    }

    pub fn set(&self, v: C) {
        *self.inner.borrow_mut() = Some(v);
    }

    pub fn get<E>(&self) -> E::Output
    where
        E: Effect,
        C: Perform<E>,
    {
        let value = self.inner.borrow_mut().take().unwrap();
        value.into()
    }
}

impl<C> Clone for Channel<C> {
    fn clone(&self) -> Self {
        Channel {
            inner: self.inner.clone(),
        }
    }
}

impl<C> Default for Channel<C> {
    fn default() -> Self {
        Channel {
            inner: Rc::new(RefCell::new(None)),
        }
    }
}

pub struct Context<E, T, C> {
    pub channel: Channel<C>,
    pub handler: Rc<Fn(E, Continuation<E, T, C>) -> T>,
}

impl<E, T, C> Clone for Context<E, T, C> {
    fn clone(&self) -> Self {
        Context {
            channel: self.channel.clone(),
            handler: self.handler.clone(),
        }
    }
}

pub struct Continuation<E, T, C> {
    expr: WithEffectInner<E, T>,
    context: Context<E, T, C>,
}

impl<E, T, C> Continuation<E, T, C> {
    pub fn run(self, v: C) -> T {
        self.context.channel.set(v);
        _handle(self.context, self.expr)
    }
}

fn _handle<E, T, C>(context: Context<E, T, C>, mut expr: WithEffectInner<E, T>) -> T {
    let state = unsafe { expr.inner.resume() };
    match state {
        GeneratorState::Yielded(effect) => {
            let handler = context.handler.clone();
            handler(effect, Continuation { expr, context })
        }
        GeneratorState::Complete(v) => v,
    }
}

pub fn handle<E, T, C, H, VH, R>(
    gen_func: Box<FnBox(Context<E, T, C>) -> WithEffectInner<E, T>>,
    value_handler: VH,
    handler: H,
) -> R
where
    H: Fn(E, Continuation<E, T, C>) -> T + 'static,
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
macro_rules! handler_muncher {
    // Open parenthesis.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `resume!($k, $e)` tokens with `resume_impl!(@$channel, $variant, $ctx, $k, $e)`.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)*) $($stack:tt)*) resume!($k:expr, $e:expr) $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(($($top)* resume_impl!(@$channel, @$variant, @$ctx, $k, $e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `resume!`.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        handler_muncher!(@$channel, @$variant, @$ctx, @(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    (@$channel:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)+))) => {{
        $($top)+
    }};
}

#[macro_export]
macro_rules! resume_impl {
    (@$channel:ident, @$variant:ident, @$ctx:ty, $k:expr, $e:expr) => {{
        $k.run($channel::$variant($e))
    }};
}

#[macro_export]
macro_rules! handler {
    ( $($variant:ident @ $eff_type:ty [ $eff:pat, $k:pat ] => $e:tt),* ) => {{
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

        enum Channel {
            $($variant(<$eff_type as Effect>::Output),)*
        }

        $(
            impl Perform<$eff_type> for Channel {
                fn into(self) -> <$eff_type as Effect>::Output {
                    match self {
                        Channel::$variant(x) => x,
                        _ => unreachable!(),
                    }
                }
            }
        )*

        move |eff: Effects, k| match eff {
            $(
                Effects::$variant($eff) => {
                    let $k = k;
                    handler_muncher!(@Channel, @$variant, @$eff_type, @(()) $e)
                }
            )*
        }
    }};
}
