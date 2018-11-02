#![feature(fnbox, nll, generators, generator_trait)]

use std::collections::HashMap;

use std::boxed::FnBox;
use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::rc::Rc;

fn fib<'a, R: 'a>(n: u32, cont: Box<Fn(u32) -> R + 'a>) -> R {
    if n <= 1 {
        cont(1)
    } else {
        fib(n - 1, Box::new(|x| fib(n - 2, Box::new(|y| cont(x + y)))))
    }
}

#[derive(Debug, Clone)]
enum Ast {
    Lit(u32),
    Add(Box<Ast>, Box<Ast>),
    Var(String),
    Fun(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

#[derive(Debug, Clone)]
enum Value {
    Int(u32),
    Fun(String, Box<Ast>),
}

fn interp<'a, R: 'a>(
    ast: &Ast,
    env: &HashMap<String, Value>,
    cont: Box<FnBox(Value) -> R + 'a>,
) -> R {
    use crate::Ast::*;
    match ast {
        Lit(x) => cont(Value::Int(*x)),
        Add(lhs, rhs) => interp(
            lhs,
            env,
            Box::new(|lhs| match lhs {
                Value::Int(lhs) => interp(
                    rhs,
                    env,
                    Box::new(|rhs| match rhs {
                        Value::Int(rhs) => cont(Value::Int(lhs + rhs)),
                        Value::Fun(_, _) => unimplemented!("type error"),
                    }),
                ),
                Value::Fun(_, _) => unimplemented!("type error"),
            }),
        ),
        Var(v) => match env.get(v).cloned() {
            Some(v) => cont(v),
            None => unimplemented!("variable not found"),
        },
        Fun(name, body) => cont(Value::Fun(name.clone(), body.clone())),
        App(f, x) => interp(
            f,
            env,
            Box::new(|f| {
                interp(
                    x,
                    env,
                    Box::new(|x| match f {
                        Value::Int(_) => unimplemented!(),
                        Value::Fun(name, body) => {
                            let mut new_env = env.clone();
                            new_env.insert(name, x);
                            interp(&body, &new_env, cont)
                        }
                    }),
                )
            }),
        ),
    }
}

enum Yield<T> {
    Val(T),
    Gen(Box<FnBox(Rc<RefCell<Option<T>>>) -> Box<dyn Generator<Yield = Yield<T>, Return = T>>>),
}

fn run_generator<'a, R: 'a, T, G: Generator<Yield = Yield<T>, Return = T>>(
    gen: &mut G,
    arg: Rc<RefCell<Option<T>>>,
    next: Box<FnBox(T) -> R + 'a>,
) -> R {
    match unsafe { gen.resume() } {
        GeneratorState::Yielded(Yield::Val(v)) => {
            *arg.borrow_mut() = Some(v);
            run_generator(gen, arg, next)
        }
        GeneratorState::Yielded(Yield::Gen(gen_func)) => {
            let inner_arg = Rc::new(RefCell::new(None));
            let mut inner_gen = gen_func(inner_arg.clone());
            run_generator(
                &mut inner_gen,
                inner_arg,
                Box::new(|result| {
                    *arg.borrow_mut() = Some(result);
                    run_generator(gen, arg, next)
                }),
            )
        }
        GeneratorState::Complete(r) => next(r),
    }
}

fn greet(
    arg: Rc<RefCell<Option<String>>>,
    name: String,
) -> impl Generator<Yield = Yield<String>, Return = String> {
    move || {
        yield Yield::Val(format!("Hi, {}", name));
        let message = arg.borrow().clone().unwrap();
        message
    }
}

fn factorial(
    arg: Rc<RefCell<Option<usize>>>,
    n: usize,
) -> Box<dyn Generator<Yield = Yield<usize>, Return = usize>> {
    Box::new(move || {
        if n == 0 {
            1
        } else {
            yield Yield::Gen(Box::new(move |arg| factorial(arg, n - 1)));
            (*arg.borrow()).unwrap() * n
        }
    })
}

fn printer<T: std::fmt::Display>() -> Box<FnBox(T) -> ()> {
    Box::new(|x| println!("{}", x))
}

fn main() {
    for x in 0..10 {
        fib(x, Box::new(|x| println!("{}", x)));
    }

    interp(
        &Ast::Lit(4),
        &HashMap::new(),
        Box::new(|x| println!("{:?}", x)),
    );
    {
        let x = Ast::Var("x".into());
        let double = Ast::Fun(
            "x".into(),
            Box::new(Ast::Add(Box::new(x.clone()), Box::new(x))),
        );
        let result = Ast::App(Box::new(double), Box::new(Ast::Lit(6)));
        interp(&result, &HashMap::new(), Box::new(|x| println!("{:?}", x)));
    }

    {
        let arg = Rc::new(RefCell::new(None));
        let mut gen = greet(arg.clone(), "hoyoyo".into());
        match unsafe { gen.resume() } {
            GeneratorState::Yielded(Yield::Val(v)) => println!("yielded: {}", v),
            _ => unreachable!(),
        }
        *arg.borrow_mut() = Some("hehehe".into());
        match unsafe { gen.resume() } {
            GeneratorState::Complete(v) => println!("complete: {}", v),
            _ => unreachable!(),
        }
    }

    {
        let arg = Rc::new(RefCell::new(None));
        run_generator(&mut greet(arg.clone(), "hoyoyo".into()), arg, printer());
    }

    {
        let arg = Rc::new(RefCell::new(None));
        run_generator(&mut factorial(arg.clone(), 10), arg, printer());
    }
}
