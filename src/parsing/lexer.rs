use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    string: &'a str,
    consumed_bytes: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            string: input,
            consumed_bytes: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ErrorKind>;

    fn next(&mut self) -> Option<Self::Item> {
        let (string, consumed) = skip_whitespace(self.string);
        self.string = string;
        self.consumed_bytes += consumed;

        if self.string.is_empty() {
            return None;
        }

        let token = integer(string).map(|Parsed(num, rest, consumed)| {
            let old_consumed_bytes = self.consumed_bytes;
            let new_consumed_bytes = old_consumed_bytes + consumed;

            self.string = rest;
            self.consumed_bytes = new_consumed_bytes;

            Token {
                at: old_consumed_bytes..new_consumed_bytes,
                lexeme: num,
                kind: TokenKind::Integer,
            }
        });

        Some(token)
    }
}

fn integer(string: &str) -> Result<Parsed<&str>, ErrorKind> {
    let (last_num, last_char) = string
        .char_indices()
        .take_while(|(_, c)| c.is_numeric())
        .last()
        .ok_or(ErrorKind::EmptyIntegerLiteral)?;

    let width = last_char.len_utf8();
    let num = &string[..last_num + width];
    let rest = &string[last_num + width..];
    let consumed = last_num + width;

    Ok(Parsed(num, rest, consumed))
}

fn skip_whitespace(string: &str) -> (&str, usize) {
    let consumed = string
        .char_indices()
        .take_while(|(_, c)| c.is_whitespace())
        .last()
        .map(|(index, c)| index + c.len_utf8())
        .unwrap_or(0);

    (&string[consumed..], consumed)
}

/// The successful result of a parsing operation
struct Parsed<'a, T>(T, &'a str, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    at: Range<usize>,
    lexeme: &'a str,
    kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Integer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    EmptyIntegerLiteral,
}

#[cfg(test)]
mod test {
    use super::*;

    fn token(at: Range<usize>, lexeme: &str, kind: TokenKind) -> Token {
        Token { at, lexeme, kind }
    }

    #[test]
    fn one_integer() {
        let mut lexer = Lexer::new("123");
        assert_eq!(
            lexer.next(),
            Some(Ok(token(0..3, "123", TokenKind::Integer)))
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn integer_sequence() {
        let lexer = Lexer::new("123 456 \u{3000} 789 ¹²³"); // \u{3000} is 3 bytes wide, and the superscript numbers are 2 bytes each

        let expected = vec![
            token(0..3, "123", TokenKind::Integer),
            token(4..7, "456", TokenKind::Integer),
            token(12..15, "789", TokenKind::Integer),
            token(16..22, "¹²³", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }
}
