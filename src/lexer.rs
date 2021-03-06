use std::iter::{Iterator, Peekable};
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,

    If,
    Then,
    Else,

    For,
    Var,
    In,

    Delimiter, //';' character
    Comma,

    OpeningParenthesis,
    ClosingParenthesis,

    Unary,
    Binary,

    Ident(String),
    Number(f64),
    Other(char),
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub(crate) input: &'a str,
    pub(crate) chars: Peekable<Chars<'a>>,
    pub(crate) pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            chars: input.chars().peekable(),
            pos: 0,
        }
    }

    fn next_byte(&mut self) -> Option<char> {
        self.pos += 1;
        self.chars.next()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let src = self.input;

        loop {
            match self.chars.peek() {
                Some(c) if !c.is_whitespace() => break,
                None => return None,
                _ => (),
            }

            self.next_byte();
        }

        let start = self.pos;

        if let Some(c) = self.next_byte() {
            match c {
                ',' => Some(Token::Comma),
                ';' => Some(Token::Delimiter),
                '(' => Some(Token::OpeningParenthesis),
                ')' => Some(Token::ClosingParenthesis),

                // identifier | keyword
                'a'...'z' | 'A'...'Z' | '_' => {
                    loop {
                        match self.chars.peek() {
                            Some(&c) if c.is_alphanumeric() || c == '_' => {
                                self.next_byte();
                            }
                            _ => break,
                        }
                    }
                    match &src[start..self.pos] {
                        "def" => Some(Token::Def),
                        "extern" => Some(Token::Extern),
                        "if" => Some(Token::If),
                        "then" => Some(Token::Then),
                        "else" => Some(Token::Else),
                        "for" => Some(Token::For),
                        "var" => Some(Token::Var),
                        "in" => Some(Token::In),
                        "unary" => Some(Token::Unary),
                        "binary" => Some(Token::Binary),
                        id => Some(Token::Ident(id.to_string())),
                    }
                }

                // number
                // FIXME: saner number pasring.
                '0'...'9' | '.' => {
                    loop {
                        match self.chars.peek() {
                            Some(&c) if c.is_digit(10) || c == '.' => self.next_byte(),
                            _ => break,
                        };
                    }
                    Some(Token::Number(
                        src[start..self.pos]
                            .parse()
                            .expect("Could not parse number!"),
                    ))
                }

                // comment
                '#' => {
                    loop {
                        match self.chars.peek() {
                            Some(&c) if c != '\r' && c != '\n' => self.next_byte(),
                            _ => break,
                        };
                    }
                    self.next()
                }
                ch if ch.is_ascii() => Some(Token::Other(ch)),

                _ => None,
            }
        } else {
            None
        }
    }
}

// Some tests for the lexer
#[cfg(test)]
mod tests {
    use super::*;
    use super::Operator::*;

    #[test]
    fn test_simple_tokens_and_value() {
        let mut lexer = Lexer::new("1 + 1 - foo");
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Operator(Add));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Operator(Sub));
        assert_eq!(lexer.next().unwrap(), Token::Ident(String::from("foo")));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_simple_tokens_and_value_no_whitespace() {
        let mut lexer = Lexer::new("1+1-foo");
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Operator(Add));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Operator(Sub));
        assert_eq!(lexer.next().unwrap(), Token::Ident(String::from("foo")));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_comments() {
        let code = "# This is a comment 1+1
        1 + 2 # <- is code
        # this is not";
        let mut lexer = Lexer::new(code);
        assert_eq!(lexer.next(), Some(Token::Number(1.0)));
        assert_eq!(lexer.next(), Some(Token::Operator(Add)));
        assert_eq!(lexer.next(), Some(Token::Number(2.0)));
        assert_eq!(lexer.next(), None);
    }
}
