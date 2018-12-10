#![feature(generators, never_type)]

extern crate eff;
use eff::*;

#[derive(Debug)]
struct ConversionError(String);
impl Effect for ConversionError {
    type Output = usize;
}

// TODO: remove 'static
fn sum_up(s: &'static str) -> usize {
    handle(
        eff! {
            let mut sum = 0_usize;
            for line in s.split('\n') {
                sum += match line.parse() {
                    Ok(x) => x,
                    Err(_) => perform!(ConversionError(s.to_string())),
                };
            }
            sum
        },
        |x| x,
        handler! {
            A @ ConversionError[eff, k] => {
                println!("conversion error: {}", eff.0);
                k.run::<ConversionError>(0)
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

    assert_eq!(sum_up(lines), 15);
}
