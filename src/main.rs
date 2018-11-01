#![feature(fnbox, nll)]

use std::collections::HashMap;

use std::boxed::FnBox;

fn fib<'a, R: 'a>(n: u32, cont: Box<Fn(u32) -> R + 'a>) -> R {
    if n <= 1 {
        cont(1)
    } else {
        fib(n - 1, Box::new(|x| {
            fib(n - 2, Box::new(|y| {
                cont(x + y)
            }))
        }))
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

fn interp<'a, R: 'a>(ast: &Ast, env: &HashMap<String, Value>, cont: Box<FnBox(Value) -> R + 'a>) -> R {
    use crate::Ast::*;
    match ast {
        Lit(x) => cont(Value::Int(*x)),
        Add(lhs, rhs) => {
            interp(lhs, env, Box::new(|lhs| {
                match lhs {
                    Value::Int(lhs) => interp(rhs, env, Box::new(|rhs| {
                        match rhs {
                            Value::Int(rhs) => cont(Value::Int(lhs + rhs)),
                            Value::Fun(_, _) => unimplemented!("type error"),
                        }
                    })),
                    Value::Fun(_, _) => unimplemented!("type error"),
                }
            }))
        },
        Var(v) => match env.get(v).cloned() {
            Some(v) => cont(v),
            None => unimplemented!("variable not found"),
        }
        Fun(name, body) => cont(Value::Fun(name.clone(), body.clone())),
        App(f, x) => interp(f, env, Box::new(|f| {
            interp(x, env, Box::new(|x| {
                match f {
                    Value::Int(_) => unimplemented!(),
                    Value::Fun(name, body) => {
                        let mut new_env = env.clone();
                        new_env.insert(name, x);
                        interp(&body, &new_env, cont)
                    }
                }
            }))
        })),
    }
}

fn main() {
    for x in 0..10 {
        fib(x, Box::new(|x| println!("{}", x)));
    }

    interp(&Ast::Lit(4), &HashMap::new(), Box::new(|x| println!("{:?}", x)));
    {
        let x = Ast::Var("x".into());
        let double = Ast::Fun("x".into(), Box::new(Ast::Add(Box::new(x.clone()), Box::new(x))));
        let result = Ast::App(Box::new(double), Box::new(Ast::Lit(6)));
        interp(&result, &HashMap::new(), Box::new(|x| println!("{:?}", x)));
    }
}
