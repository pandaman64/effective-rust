//! 1.1. Recovering from errors

#![feature(generators, generator_trait, try_from, never_type)]

use eff::*;

#[derive(Debug)]
struct ConversionError(String);
impl Effect for ConversionError {
    type Output = usize;
}

fn sum_up(s: String) -> usize {
    #[eff(ConversionError)]
    fn read(s: String) -> usize {
        let mut sum = 0_usize;
        for line in s.split('\n') {
            sum += match line.parse() {
                Ok(x) => x,
                Err(_e) => perform!(ConversionError(line.to_string())),
            }
        }
        sum
    }

    let e = read(s);
    e.handle(|x| {
        println!("conversion error: {:?}", x);
        HandlerResult::Resume(0)
    })
    .run(|x| x)
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
