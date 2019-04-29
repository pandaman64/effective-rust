#![feature(generators, generator_trait, never_type)]

use eff::{eff, handler, Effect, Effectful};
use std::cell::{Cell, RefCell};
use std::io::Read;

struct NextByte;

impl Effect for NextByte {
    type Output = Option<u8>;
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
struct WordCount {
    bytes: usize,
    lines: usize,
    words: usize,
}

#[eff(NextByte)]
fn count_word() -> WordCount {
    let mut bytes = 0;
    let mut lines = 0;
    let mut words = 0;

    let mut is_previous_space = true; // Is the previous byte space?

    while let Some(b) = eff::perform!(NextByte) {
        bytes += 1;

        // count newline
        if b == b'\n' {
            lines += 1;
        }

        // encountered new word
        if is_previous_space && !b.is_ascii_whitespace() {
            words += 1;
        }

        is_previous_space = b.is_ascii_whitespace();
    }

    WordCount {
        bytes,
        lines,
        words,
    }
}

fn main() {
    // count words from str
    let s: Cell<&[u8]> = Cell::new(b"Hello, \tWorld!\n");
    let s = &s;

    assert_eq!(
        count_word()
            .handle(handler! {
                x => x,
                NextByte, k => {
                    if s.get().len() == 0 {
                        eff::perform!(k.resume(None))
                    } else {
                        let b = s.get()[0];
                        s.set(&s.get()[1..]);
                        eff::perform!(k.resume(Some(b)))
                    }
                }
            })
            .block_on(),
        WordCount {
            bytes: 15,
            lines: 1,
            words: 2,
        }
    );

    // count words from stdin
    let stdin = std::io::stdin();
    let stdin = RefCell::new(stdin.lock());
    let stdin = &stdin;
    let buffer = RefCell::new([0; 1]);
    let buffer = &buffer;

    let words = count_word()
        .handle(handler! {
            x => x,
            NextByte, k => {
                let mut stdin = stdin.borrow_mut();
                let mut buffer = buffer.borrow_mut();
                match stdin.read(&mut *buffer) {
                    Ok(1) => {
                        let b = (&mut *buffer)[0];
                        drop(stdin);
                        drop(buffer);
                        eff::perform!(k.resume(Some(b)))
                    },
                    _ => eff::perform!(k.resume(None)),
                }
            }
        })
        .block_on();
    println!("{:?}", words);
}
