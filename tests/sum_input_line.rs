//! 1.1. Recovering from errors

#![feature(generators, never_type)]

extern crate eff;
use eff::*;

#[derive(Debug)]
struct ConversionError(String);
impl Effect for ConversionError {
    type Output = usize;
}

fn sum_up(s: String) -> usize {
    handle(
        eff! {
            let mut sum = 0_usize;
            for line in s.split('\n') {
                sum += match line.parse() {
                    Ok(x) => x,
                    Err(_e) => perform!(ConversionError(line.to_string())),
                }
            }
            sum
        },
        |x| x,
        handler! {
            A @ ConversionError[eff] => {
                println!("conversion error: {}", eff.0);
                resume!(0)
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
