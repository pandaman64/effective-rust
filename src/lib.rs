#![feature(nll, generators, generator_trait, unsized_locals)]
#![feature(trace_macros)]

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

// https://users.rust-lang.org/t/macro-to-replace-type-parameters/17903
#[macro_export]
macro_rules! eff_muncher {
    // Open parenthesis.
    ($us:ident, @($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        eff_muncher!($us, @(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    ($us:ident, @($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        eff_muncher!($us, @(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    ($us:ident, @($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        eff_muncher!($us, @(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    ($us:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    ($us:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    ($us:ident, @(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `perform!($e)` tokens with `perform!($e)`.
    ($us:ident, @(($($top:tt)*) $($stack:tt)*) perform!($e:expr) $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* perform!($e)) $($stack)*) $($rest)*)
    };

    // Replace `compose!($variant, $e)` tokens with `compose!($us, $variant, $e)`.
    ($us:ident, @(($($top:tt)*) $($stack:tt)*) compose!($variant:ident, $e:expr) $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* compose!($us, $variant, $e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `perform!` nor `invoke!`.
    ($us:ident, @(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        eff_muncher!($us, @(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    ($us:ident, @(($($top:tt)+))) => {{
        $($top)+
    }};
}

#[macro_export]
macro_rules! eff {
    // Begin with an empty stack.
    ($($input:tt)+) => {{
        enum Us {}

        $crate::WithEffectInner {
            inner: static move || {
                // This trick lets the compiler treat this closure
                // as a generator even if $input doesn't contain no perform
                // (no yield).
                // see: https://stackoverflow.com/a/53757228/8554666
                if false { yield unreachable!(); }
                eff_muncher!(Us, @(()) $($input)*)
            }
        }
    }};
}

#[macro_export]
macro_rules! eff_with_compose {
    // Begin with an empty stack.
    (<$($variant:ident @ $type:ty),*>, $($input:tt)+) => {{
        enum Us {
            $($variant($type)),*
        }

        $(
            impl From<$type> for Us {
                fn from(v: $type) -> Self {
                    Us::$variant(v)
                }
            }
        )*

        $crate::WithEffectInner {
            inner: static move || {
                // This trick lets the compiler treat this closure
                // as a generator even if $input doesn't contain no perform
                // (no yield).
                // see: https://stackoverflow.com/a/53757228/8554666
                if false { yield unreachable!(); }
                eff_muncher!(Us, @(()) $($input)*)
            }
        }
    }};
}

#[macro_export]
macro_rules! perform {
    ($eff:expr) => {{
        #[inline(always)]
        fn __getter<'e, 'c, E: $crate::Effect, C: $crate::Channel<E> + 'c>(
            _: &'e E,
            store: $crate::Store<C>,
        ) -> impl FnOnce() -> <E as $crate::Effect>::Output + 'c {
            move || store.get::<E>()
        }
        let store = Store::new();
        let eff = $eff;
        let getter = __getter(&eff, store.clone());
        yield $crate::Suspension::Perform(store.clone(), Into::into(eff));
        getter()
    }};
}

#[macro_export]
macro_rules! compose {
    ($Us:ident, $variant:ident, $eff:expr) => {{
        let store = ComposeStore::new();
        let store2 = store.clone();
        let eff = $eff;
        yield $crate::Suspension::Compose(Box::new(move |handler| {
            $crate::run_inner(eff, store, handler)
        }));
        match store2.take() {
            $Us::$variant(v) => v,
            _ => unreachable!(),
        }
    }};
}

pub enum Suspension<E, C, R> {
    Perform(Store<C>, E),
    Compose(Box<FnOnce(&mut FnMut(E) -> HandlerResult<R, C>) -> Option<R>>),
}

pub trait Effect {
    type Output;
}

pub struct WithEffectInner<G> {
    pub inner: G,
}

pub trait Channel<E>
where
    E: Effect,
{
    fn into(self) -> E::Output;
}

pub struct ComposeStore<U> {
    pub inner: Rc<RefCell<Option<U>>>,
}

impl<U> Clone for ComposeStore<U> {
    fn clone(&self) -> Self {
        ComposeStore {
            inner: self.inner.clone(),
        }
    }
}

impl<U> Default for ComposeStore<U> {
    fn default() -> Self {
        ComposeStore {
            inner: Default::default(),
        }
    }
}

impl<U> ComposeStore<U> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set(&self, v: U) {
        *self.inner.borrow_mut() = Some(v);
    }

    pub fn take(&self) -> U {
        self.inner.borrow_mut().take().unwrap()
    }
}

#[derive(Debug)]
pub struct Store<C> {
    pub inner: Rc<RefCell<Option<C>>>,
}

impl<C> Store<C> {
    pub fn new() -> Self {
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

pub fn run_inner<G, E, U, Us, C, R>(
    mut expr: WithEffectInner<G>,
    store: ComposeStore<Us>,
    handler: &mut FnMut(E) -> HandlerResult<R, C>,
) -> Option<R>
where
    G: Generator<Yield = Suspension<E, C, R>, Return = U>,
    U: Into<Us>,
{
    loop {
        let state = unsafe { expr.inner.resume() };
        match state {
            GeneratorState::Yielded(Suspension::Perform(store, effect)) => match handler(effect) {
                HandlerResult::Resume(c) => store.set(c),
                HandlerResult::Exit(v) => return Some(v),
            },
            GeneratorState::Yielded(Suspension::Compose(f)) => {
                return f(handler);
            }
            GeneratorState::Complete(v) => {
                store.set(v.into());
                return None;
            }
        }
    }
}

pub fn run<G, E, T, C, H, VH, R>(
    mut expr: WithEffectInner<G>,
    value_handler: VH,
    mut handler: H,
) -> R
where
    G: Generator<Yield = Suspension<E, C, R>, Return = T>,
    H: FnMut(E) -> HandlerResult<R, C> + 'static,
    VH: FnOnce(T) -> R,
{
    loop {
        // this resume is safe since the generator is pinned in a heap
        let state = unsafe { expr.inner.resume() };
        match state {
            GeneratorState::Yielded(Suspension::Perform(store, effect)) => match handler(effect) {
                HandlerResult::Resume(c) => store.set(c),
                HandlerResult::Exit(v) => return v,
            },
            GeneratorState::Yielded(Suspension::Compose(f)) => {
                if let Some(v) = f(&mut handler) {
                    return v;
                }
            }
            GeneratorState::Complete(v) => return value_handler(v),
        }
    }
}

pub enum HandlerResult<R, C> {
    Resume(C),
    Exit(R),
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
