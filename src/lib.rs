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

    // Replace `perform!($e)` tokens with `perform!(@$ctx, $e)`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) perform!($e:expr) $($rest:tt)*) => {
        eff_muncher!(@$ctx, @(($($top)* perform!(@$ctx, $e)) $($stack)*) $($rest)*)
    };

    // Replace `invoke!($e)` tokens with `invoke!(@$ctx, $e)`.
    // (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) invoke!($e:expr) $($rest:tt)*) => {
    //     eff_muncher!(@$ctx, @(($($top)* invoke!(@$ctx, $e)) $($stack)*) $($rest)*)
    // };

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
                    static move || {
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
macro_rules! perform {
    (@$ctx:ident, $eff:expr) => {{
        #[inline(always)]
        fn __getter<'e, 'c, E: Effect, C: Channel<E> + 'c>(
            _: &'e E,
            store: Store<C>,
        ) -> impl FnOnce() -> <E as Effect>::Output + 'c {
            move || store.get::<E>()
        }
        let eff = $eff;
        let getter = __getter(&eff, $ctx.store.clone());
        yield Into::into(eff);
        getter()
    }};
}

pub type WithEffect<E, T, C> = Box<FnBox(Context<E, T, C>) -> WithEffectInner<E, T>>;

pub trait Effect {
    type Output;
}

pub struct WithEffectInner<E, T> {
    pub inner: Box<dyn Generator<Yield = E, Return = T>>,
}

pub trait Channel<E>
where
    E: Effect,
{
    fn into(self) -> E::Output;
}

#[derive(Debug)]
pub struct Store<C> {
    pub inner: Rc<RefCell<Option<C>>>,
}

impl<C> Store<C> {
    fn new() -> Self {
        Default::default()
    }

    pub fn set(&self, v: C) {
        *self.inner.borrow_mut() = Some(v);
    }

    pub fn get<E>(&self) -> E::Output
    where
        E: Effect,
        C: Channel<E>,
    {
        let value = self.inner.borrow_mut().take().unwrap();
        value.into()
    }
}

impl<C> Clone for Store<C> {
    fn clone(&self) -> Self {
        Store {
            inner: self.inner.clone(),
        }
    }
}

impl<C> Default for Store<C> {
    fn default() -> Self {
        Store {
            inner: Rc::new(RefCell::new(None)),
        }
    }
}

pub struct Context<E, T, C> {
    pub store: Store<C>,
    pub handler: Rc<RefCell<dyn FnMut(E) -> HandlerResult<T, C>>>,
}

impl<E, T, C> Clone for Context<E, T, C> {
    fn clone(&self) -> Self {
        Context {
            store: self.store.clone(),
            handler: self.handler.clone(),
        }
    }
}

fn _handle<E, T, C>(context: Context<E, T, C>, mut expr: WithEffectInner<E, T>) -> T {
    loop {
        let state = unsafe { expr.inner.resume() };
        match state {
            GeneratorState::Yielded(effect) => {
                let handler = &mut *context.handler.borrow_mut();
                match handler(effect) {
                    HandlerResult::Resume(c) => context.store.set(c),
                    HandlerResult::Exit(v) => return v,
                }
            }
            GeneratorState::Complete(v) => return v,
        }
    }
}

pub fn handle<E, T, C, H, VH, R>(
    gen_func: Box<dyn FnBox(Context<E, T, C>) -> WithEffectInner<E, T>>,
    value_handler: VH,
    handler: H,
) -> R
where
    H: FnMut(E) -> HandlerResult<T, C> + 'static,
    VH: FnOnce(T) -> R,
{
    let context = Context {
        store: Store::new(),
        handler: Rc::new(RefCell::new(handler)),
    };
    let expr = gen_func(context.clone());
    value_handler(_handle(context, expr))
}

pub enum HandlerResult<T, C> {
    Resume(C),
    Exit(T),
}

#[macro_export]
macro_rules! handler_muncher {
    // Open parenthesis.
    (@$store:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    (@$store:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    (@$store:ident, @$variant:ident, @$ctx:ty, @($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `resume!($e)` tokens with `resume!(@$store, $variant, $ctx, $e)`.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)*) $($stack:tt)*) resume!($e:expr) $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(($($top)* resume!(@$store, @$variant, @$ctx, $e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `resume!`.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        handler_muncher!(@$store, @$variant, @$ctx, @(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    (@$store:ident, @$variant:ident, @$ctx:ty, @(($($top:tt)+))) => {{
        $($top)+
    }};
}

#[macro_export]
macro_rules! resume {
    (@$store:ident, @$variant:ident, @$ctx:ty, $e:expr) => {{
        return $crate::HandlerResult::Resume($store::$variant($e));
        unreachable!()
    }};
}

#[macro_export]
macro_rules! handler {
    ( $($variant:ident @ $eff_type:ty [ $eff:pat ] => $e:tt),* ) => {{
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

        enum Channels {
            $($variant(<$eff_type as Effect>::Output),)*
        }

        $(
            impl $crate::Channel<$eff_type> for Channels {
                fn into(self) -> <$eff_type as Effect>::Output {
                    match self {
                        Channels::$variant(x) => x,
                        _ => unreachable!(),
                    }
                }
            }
        )*

        #[allow(unreachable_code)]
        move |eff: Effects| match eff {
            $(
                Effects::$variant($eff) => {
                    $crate::HandlerResult::Exit(handler_muncher!(@Channels, @$variant, @$eff_type, @(()) $e))
                }
            )*
        }
    }};
}
