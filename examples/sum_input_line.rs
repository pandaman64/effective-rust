#![feature(generators, never_type, impl_trait_in_bindings)]

use eff::{eff, handler, perform, Effect, Effectful};

struct ConversionError<'a>(&'a str);

impl Effect for ConversionError<'_> {
    type Output = usize;
}

#[eff(ConversionError)]
fn sum_lines(s: &str) -> usize {
    let lines = s.split('\n');
    let mut sum = 0;

    for line in lines {
        match line.parse::<usize>() {
            Ok(x) => sum += x,
            Err(_e) => sum += perform!(ConversionError(line)),
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

    if matches.is_present("panic") {
        let sum = sum_comp
            .handle(handler! {
                x => x,
                ConversionError(s), _k => panic!("not an integer: {}", s),
            })
            .block_on();

        println!("sum = {}", sum);
    } else if matches.is_present("result") {
        let sum = sum_comp
            .handle(handler! {
                x => Ok(x),
                ConversionError(s), _k => Err(format!("not an integer: {}", s)),
            })
            .block_on();

        println!("sum = {:?}", sum);
    } else if matches.is_present("zero") {
        let sum = sum_comp
            .handle(handler! {
                x => x,
                ConversionError(s), k => {
                    eprintln!("not an integer: {}", s);
                    perform!(k.resume(0))
                }
            })
            .block_on();

        println!("sum = {}", sum);
    }
}
