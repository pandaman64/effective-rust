//! 1.1. Recovering from errors

#![feature(generators, generator_trait, never_type)]

use eff::*;

#[derive(Debug)]
struct ConversionError<'a>(&'a str);
impl Effect for ConversionError<'_> {
    type Output = usize;
}

fn sum_up(s: &str) -> usize {
    #[eff(ConversionError)]
    fn read(s: &str) -> usize {
        let mut sum = 0_usize;
        for line in s.split('\n') {
            sum += match line.parse() {
                Ok(x) => x,
                Err(_e) => perform!(ConversionError(line)),
            }
        }
        sum
    }

    let e = read(s);
    e.handle(
        |x| pure(x).embed(),
        |e| {
            e.on(|ConversionError(x), k| {
                effectful! {
                    println!("conversion error: {:?}", x);
                    perform!(k.resume(0))
                }
            })
        },
    )
    .block_on()
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
