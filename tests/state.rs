//! 2. Effectful Computations in a Pure Setting

#![feature(fnbox, generators, never_type)]

extern crate eff;
use eff::*;

use std::boxed::FnBox;

struct GetInt;
impl Effect for GetInt {
    type Output = u32;
}

struct SetInt(u32);
impl Effect for SetInt {
    type Output = ();
}

struct UpdateInt(Box<FnBox(u32) -> u32>);
impl Effect for UpdateInt {
    type Output = ();
}

struct GetStr;
impl Effect for GetStr {
    type Output = String;
}

struct SetStr(String);
impl Effect for SetStr {
    type Output = ();
}

#[test]
fn test_state() {
    let mut i = 42;
    let mut s = "hello".to_string();
    handle(
        eff! {
            println!("{}", perform!(GetInt));
            perform!(SetInt(53));
            println!("{}", perform!(GetInt));
            perform!(UpdateInt(Box::new(|x| x * 2)));
            println!("{}", perform!(GetInt));
            println!("{}", perform!(GetStr));
            perform!(SetStr("world".into()));
            println!("{}", perform!(GetStr));
        },
        |x| x,
        handler! {
            GI @ GetInt[_, k] => {
                resume!(k, i)
            },
            SI @ SetInt[SetInt(x), k] => {
                i = x;
                resume!(k, ())
            },
            UI @ UpdateInt[UpdateInt(f), k] => {
                i = f(i);
                resume!(k, ())
            },
            GS @ GetStr[_, k] => {
                resume!(k, s.clone())
            },
            SS @ SetStr[SetStr(x), k] => {
                s = x;
                resume!(k, ())
            }
        },
    )
}
