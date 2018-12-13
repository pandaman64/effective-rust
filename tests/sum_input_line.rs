//! 1.1. Recovering from errors

#![feature(generators, never_type)]

extern crate eff;
use eff::*;

#[derive(Debug)]
struct ConversionError(String);
impl Effect for ConversionError {
    type Output = usize;
}

struct Exit(usize);
impl Effect for Exit {
    type Output = usize;
}

fn parse<E: From<ConversionError> + 'static>(line: String) -> WithEffect<E, usize> {
    eff! {
        match line.parse() {
            Ok(x) => x,
            Err(_) => perform!(ConversionError(line.to_string())),
        }
    }
}

fn sum_up(s: String) -> usize {
    handle(
        eff! {
            let mut sum = 0_usize;
            for line in s.split('\n') {
                sum += invoke!(parse(line.to_string()));
            }
            perform!(Exit(sum));
            unreachable!()
        },
        |x| x,
        handler! {
            A @ ConversionError[eff, k] => {
                println!("conversion error: {}", eff.0);
                k.run::<ConversionError>(0)
            },
            B @ Exit[Exit(x), _k] => {
                x
            }
        },
    )
}

#[test]
fn test_lines() {
    let lines = r#"
1
2
3
foo
4
5

"#;

    assert_eq!(sum_up(lines.to_string()), 15);
}
