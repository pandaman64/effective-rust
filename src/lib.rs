#![feature(nll, generators, generator_trait, unsized_locals)]
#![feature(trace_macros)]

use std::any::Any;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

// https://users.rust-lang.org/t/macro-to-replace-type-parameters/17903
#[macro_export]
macro_rules! eff_muncher {
    // Open parenthesis.
    (@($($stack:tt)*) ($($first:tt)*) $($rest:tt)*) => {
        eff_muncher!(@(() $($stack)*) $($first)* __paren $($rest)*)
    };

    // Open square bracket.
    (@($($stack:tt)*) [$($first:tt)*] $($rest:tt)*) => {
        eff_muncher!(@(() $($stack)*) $($first)* __bracket $($rest)*)
    };

    // Open brace.
    (@($($stack:tt)*) {$($first:tt)*} $($rest:tt)*) => {
        eff_muncher!(@(() $($stack)*) $($first)* __brace $($rest)*)
    };

    // Close parenthesis.
    (@(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __paren $($rest:tt)*) => {
        eff_muncher!(@(($($top)* ($($close)*)) $($stack)*) $($rest)*)
    };

    // Close square bracket.
    (@(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __bracket $($rest:tt)*) => {
        eff_muncher!(@(($($top)* [$($close)*]) $($stack)*) $($rest)*)
    };

    // Close brace.
    (@(($($close:tt)*) ($($top:tt)*) $($stack:tt)*) __brace $($rest:tt)*) => {
        eff_muncher!(@(($($top)* {$($close)*}) $($stack)*) $($rest)*)
    };

    // Replace `perform!($e)` tokens with `perform!($e)`.
    (@(($($top:tt)*) $($stack:tt)*) perform!($e:expr) $($rest:tt)*) => {
        eff_muncher!(@(($($top)* perform!($e)) $($stack)*) $($rest)*)
    };

    // Replace `compose!($variant, $e)` tokens with `compose!($us, $variant, $e)`.
    (@$ctx:ident, @(($($top:tt)*) $($stack:tt)*) compose!($e:expr) $($rest:tt)*) => {
        eff_muncher!(@(($($top)* invoke!($e)) $($stack)*) $($rest)*)
    };

    // Munch a token that is not `perform!` nor `invoke!`.
    (@(($($top:tt)*) $($stack:tt)*) $first:tt $($rest:tt)*) => {
        eff_muncher!(@(($($top)* $first) $($stack)*) $($rest)*)
    };

    // Done.
    (@(($($top:tt)+))) => {{
        $($top)+
    }};
}

#[macro_export]
macro_rules! eff {
    // Begin with an empty stack.
    ($($input:tt)+) => {{
        $crate::WithEffectInner {
            inner: Box::new(
                #[allow(unreachable_code)]
                static move || {
                    // This trick lets the compiler treat this closure
                    // as a generator even if $input doesn't contain no perform
                    // (no yield).
                    // see: https://stackoverflow.com/a/53757228/8554666
                    if false { yield unreachable!(); }
                    eff_muncher!(@(()) $($input)*)
                }
            )
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

        $crate::WithEffectInner {
            inner: Box::new(
                #[allow(unreachable_code)]
                static move || {
                    // This trick lets the compiler treat this closure
                    // as a generator even if $input doesn't contain no perform
                    // (no yield).
                    // see: https://stackoverflow.com/a/53757228/8554666
                    if false { yield unreachable!(); }
                    eff_muncher!(@(()) $($input)*)
                }
            )
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
    ($eff:expr) => {{
        use std::any::Any;
        #[inline(always)]
        fn __getter<E, U: Any, C>(
            _: &WithEffectInner<E, U, C>,
            store: $crate::ComposeStore,
        ) -> impl FnOnce() -> U {
            move || store.take::<U>()
        }
        let store = ComposeStore::new();
        let eff = $eff;
        let getter = __getter(&eff, store.clone());
        yield $crate::Suspension::Compose(store.clone(), eff.map(|x| Box::new(x) as Box<dyn Any>));
        getter()
    }};
}

pub enum Suspension<E, C> {
    Perform(Store<C>, E),
    Compose(ComposeStore, WithEffectInner<E, Box<dyn Any>, C>),
}

pub trait Effect {
    type Output;
}

pub struct WithEffectInner<E, T, C> {
    pub inner: Box<dyn Generator<Yield = Suspension<E, C>, Return = T>>,
}

struct Map<E, T, C, F> {
    inner: WithEffectInner<E, T, C>,
    f: Option<F>,
}

impl<E, T, C, F, U> Generator for Map<E, T, C, F>
where
    F: FnOnce(T) -> U,
{
    type Yield = Suspension<E, C>;
    type Return = U;

    unsafe fn resume(&mut self) -> GeneratorState<Self::Yield, Self::Return> {
        let state = self.inner.inner.resume();
        match state {
            GeneratorState::Complete(v) => {
                let f = self.f.take().expect("Map::resume called after completion");
                GeneratorState::Complete(f(v))
            }
            GeneratorState::Yielded(v) => GeneratorState::Yielded(v),
        }
    }
}

impl<E: 'static, T: 'static, C: 'static> WithEffectInner<E, T, C> {
    pub fn map<F, U>(self, f: F) -> WithEffectInner<E, U, C>
    where
        F: FnOnce(T) -> U + 'static,
    {
        let mapped = Map {
            inner: self,
            f: Some(f),
        };

        WithEffectInner {
            inner: Box::new(mapped),
        }
    }
}

pub trait Channel<E>
where
    E: Effect,
{
    fn into(self) -> E::Output;
}

#[derive(Clone, Default)]
pub struct ComposeStore {
    pub inner: Rc<RefCell<Option<Box<dyn Any>>>>,
}

impl ComposeStore {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set<V: Any>(&self, v: V) {
        *self.inner.borrow_mut() = Some(Box::new(v));
    }

    pub fn take<V: Any>(&self) -> V {
        *self
            .inner
            .borrow_mut()
            .take()
            .unwrap()
            .downcast::<V>()
            .unwrap()
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

fn run_inner<E, U, C, H, R>(
    mut expr: WithEffectInner<E, U, C>,
    store: ComposeStore,
    handler: &mut H,
) -> Option<R>
where
    U: Any,
    H: FnMut(E) -> HandlerResult<R, C>,
{
    loop {
        let state = unsafe { expr.inner.resume() };
        match state {
            GeneratorState::Yielded(Suspension::Perform(store, effect)) => match handler(effect) {
                HandlerResult::Resume(c) => store.set(c),
                HandlerResult::Exit(v) => return Some(v),
            },
            GeneratorState::Yielded(Suspension::Compose(store, inner)) => {
                if let Some(v) = run_inner(inner, store, handler) {
                    return Some(v);
                }
            }
            GeneratorState::Complete(v) => {
                store.set(v);
                return None;
            }
        }
    }
}

pub fn run<E, T, C, H, VH, R>(
    mut expr: WithEffectInner<E, T, C>,
    value_handler: VH,
    mut handler: H,
) -> R
where
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
            GeneratorState::Yielded(Suspension::Compose(store, inner)) => {
                if let Some(v) = run_inner(inner, store, &mut handler) {
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
