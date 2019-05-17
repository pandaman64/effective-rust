#![feature(generators, never_type, impl_trait_in_bindings)]

use eff::{eff, handler, perform, Effect, Effectful};

struct NotAnInteger<'a>(&'a str);

impl Effect for NotAnInteger<'_> {
    type Output = usize;
}

#[eff(NotAnInteger)]
fn sum_lines(s: &str) -> usize {
    let lines = s.split('\n');
    let mut sum = 0;

    for line in lines {
        match line.parse::<usize>() {
            Ok(x) => sum += x,
            Err(_e) => sum += perform!(NotAnInteger(line)),
        }
    }

    sum
}

fn main() {
    use clap::{App, ArgGroup};

    let matches = App::new("sum_input_line")
        .args_from_usage(
            "--panic
             --result
             --zero",
        )
        .group(
            ArgGroup::with_name("behavior")
                .args(&["panic", "result", "zero"])
                .required(true),
        )
        .get_matches();

    let sum_comp = sum_lines("1\n2\nthree\n4\n5");
    let handled;

    if matches.is_present("panic") {
        handled = sum_comp
            .handle(handler! {
                x => Ok(x),
                NotAnInteger(s), _k => panic!("not an integer: {}", s),
            })
            .boxed();
    } else if matches.is_present("result") {
        handled = sum_comp
            .handle(handler! {
                x => Ok(x),
                NotAnInteger(s), _k => Err(format!("not an integer: {}", s)),
            })
            .boxed();
    } else if matches.is_present("zero") {
        handled = sum_comp
            .handle(handler! {
                x => Ok(x),
                NotAnInteger(s), k => {
                    eprintln!("not an integer: {}", s);
                    perform!(k.resume(0))
                }
            })
            .boxed();
    } else {
        unreachable!()
    }

    let sum = handled.block_on();

    println!("sum = {:?}", sum);
}
