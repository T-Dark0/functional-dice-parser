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

    fn advance_and_make_token(
        &mut self,
        lexeme: &'a str,
        rest: &'a str,
        consumed: usize,
        kind: TokenKind,
    ) -> Token<'a> {
        let old_consumed_bytes = self.consumed_bytes;
        let new_consumed_bytes = old_consumed_bytes + consumed;

        self.string = rest;
        self.consumed_bytes = new_consumed_bytes;

        Token {
            at: old_consumed_bytes..new_consumed_bytes,
            lexeme,
            kind,
        }
    }
}

macro_rules! make_lexer_sequence {
    (
        $self:ident, $string:ident;
        $first_function:ident $(($first_ch:expr))?, $first_kind:expr;
        $($function:ident $(($ch:expr))?, $kind:expr;)*
    ) => {
        if let Some(Parsed(lexeme, rest, consumed)) = $first_function($string, $($first_ch)?) {
            $self.advance_and_make_token(lexeme, rest, consumed, $first_kind)
        }
        $(
            else if let Some(Parsed(lexeme, rest, consumed)) = $function($string, $($ch)?) {
                $self.advance_and_make_token(lexeme, rest, consumed, $kind)
            }
        )*
        else {
            todo!()
        }
    };
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (string, consumed) = skip_whitespace(self.string);
        self.string = string;
        self.consumed_bytes += consumed;

        if self.string.is_empty() {
            return None;
        }

        let token = make_lexer_sequence!(
            self, string;
            integer, TokenKind::Integer;
            d_token, TokenKind::D;
            single_char('+'), TokenKind::Plus;
            single_char('-'), TokenKind::Minus;
            single_char('*'), TokenKind::Star;
            single_char('/'), TokenKind::Slash;
        );

        Some(token)
    }
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

fn integer(string: &str) -> Option<Parsed> {
    let (last_num, last_char) = string
        .char_indices()
        .take_while(|(_, c)| c.is_numeric())
        .last()?;

    let width = last_char.len_utf8();
    let num = &string[..last_num + width];
    let rest = &string[last_num + width..];
    let consumed = last_num + width;

    Some(Parsed(num, rest, consumed))
}

fn d_token(string: &str) -> Option<Parsed> {
    if string.starts_with("d") || string.starts_with("D") {
        Some(Parsed(&string[..1], &string[1..], 1))
    } else {
        None
    }
}

fn single_char(string: &str, ch: char) -> Option<Parsed> {
    if string.starts_with(ch) {
        Some(Parsed(&string[..1], &string[1..], 1))
    } else {
        None
    }
}

/// The successful result of a parsing operation
struct Parsed<'a>(&'a str, &'a str, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    at: Range<usize>,
    lexeme: &'a str,
    kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Integer,
    D,
    Plus,
    Minus,
    Star,
    Slash,
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
        assert_eq!(lexer.next(), Some(token(0..3, "123", TokenKind::Integer)));
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
        assert!(lexer.eq(expected.into_iter()));
    }

    #[test]
    fn d_tokens() {
        let mut lexer = Lexer::new("dD");
        assert_eq!(lexer.next(), Some(token(0..1, "d", TokenKind::D)));
        assert_eq!(lexer.next(), Some(token(1..2, "D", TokenKind::D)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn dice_expression() {
        let lexer = Lexer::new("15d7");
        let expected = vec![
            token(0..2, "15", TokenKind::Integer),
            token(2..3, "d", TokenKind::D),
            token(3..4, "7", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter()));
    }

    #[test]
    fn arithmetics() {
        let lexer = Lexer::new("1 + 2 - 3 * 4 / 5");
        let expected = vec![
            token(0..1, "1", TokenKind::Integer),
            token(2..3, "+", TokenKind::Plus),
            token(4..5, "2", TokenKind::Integer),
            token(6..7, "-", TokenKind::Minus),
            token(8..9, "3", TokenKind::Integer),
            token(10..11, "*", TokenKind::Star),
            token(12..13, "4", TokenKind::Integer),
            token(14..15, "/", TokenKind::Slash),
            token(16..17, "5", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter()));
    }

    #[test]
    fn dice_expression_with_bonus() {
        let lexer = Lexer::new("6d6 + 3");
        let expected = vec![
            token(0..1, "6", TokenKind::Integer),
            token(1..2, "d", TokenKind::D),
            token(2..3, "6", TokenKind::Integer),
            token(4..5, "+", TokenKind::Plus),
            token(6..7, "3", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter()));
    }

    #[test]
    fn negative_number() {
        let lexer = Lexer::new("-13");
        let expected = vec![
            token(0..1, "-", TokenKind::Minus),
            token(1..3, "13", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter()));
    }
}
