#![feature(nll, generators, generator_trait, unsized_locals)]
#![feature(trace_macros)]

use std::cell::RefCell;
use std::marker::PhantomData;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use pin_utils::unsafe_pinned;
use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub use eff_attr::eff;

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
        let store = $crate::Store::new();
        let eff = $eff;
        let getter = __getter(&eff, store.clone());
        yield $crate::Suspension::Perform(store.clone(), Into::into(eff));
        getter()
    }};
}

#[macro_export]
macro_rules! compose {
    ($eff:expr) => {{
        let store = $crate::ComposeStore::new();
        let store2 = store.clone();
        let eff = $eff;
        yield $crate::Suspension::Compose(Box::new(move |handler| {
            $crate::run_inner(eff, store, handler)
        }));
        store2.take()
    }};
}

#[proc_macro_hack::proc_macro_hack]
pub use eff_attr::handler;

#[macro_export]
macro_rules! resume {
    (@$eff_type:ty, $e:expr) => {{
        return eff::HandlerResult::Resume(eff::Channel::<$eff_type>::from($e));
    }};
}

#[macro_export]
macro_rules! exit {
    ($e:expr) => {{
        return eff::HandlerResult::Exit($e);
    }};
}

pub enum Suspension<E, C, R> {
    Perform(Store<C>, E),
    Compose(Box<FnOnce(&Fn(E) -> HandlerResult<R, C>) -> Option<R>>),
}

pub trait Effect {
    type Output;
}

pub struct WithEffectInner<PE, PC, G> {
    pub inner: G,
    phantom: PhantomCovariantAlwaysSendSync<fn() -> (PE, PC)>,
}

impl<PE, PC, G> WithEffectInner<PE, PC, G> {
    unsafe_pinned!(inner: G);

    pub fn new(inner: G) -> Self {
        WithEffectInner {
            inner,
            phantom: PhantomData,
        }
    }
}

pub trait Channel<E>
where
    E: Effect,
{
    fn from(v: E::Output) -> Self;
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

pub fn run_inner<G, E, U, C, R>(
    mut expr: Pin<&mut WithEffectInner<E, C, G>>,
    store: ComposeStore<U>,
    handler: &Fn(E) -> HandlerResult<R, C>,
) -> Option<R>
where
    G: Generator<Yield = Suspension<E, C, R>, Return = U>,
{
    loop {
        let state = expr.as_mut().inner().resume();
        match state {
            GeneratorState::Yielded(Suspension::Perform(store, effect)) => match handler(effect) {
                HandlerResult::Resume(c) => store.set(c),
                HandlerResult::Exit(v) => return Some(v),
                HandlerResult::Unhandled => panic!("effect unhandled"),
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
    mut expr: Pin<&mut WithEffectInner<E, C, G>>,
    value_handler: VH,
    mut handler: H,
) -> R
where
    G: Generator<Yield = Suspension<E, C, R>, Return = T>,
    H: Fn(E) -> HandlerResult<R, C> + 'static,
    VH: FnOnce(T) -> R,
{
    loop {
        // this resume is safe since the generator is pinned in a heap
        // FIXME: the above line is not the case since G might be &mut Generator.
        // Wait until pinning API arrives
        let state = expr.as_mut().inner().resume();
        match state {
            GeneratorState::Yielded(Suspension::Perform(store, effect)) => match handler(effect) {
                HandlerResult::Resume(c) => store.set(c),
                HandlerResult::Exit(v) => return v,
                HandlerResult::Unhandled => panic!("effect unhandled"),
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
    Unhandled,
}
