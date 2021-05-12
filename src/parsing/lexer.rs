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

    fn advance_and_make_token(&mut self, parsed: Parsed<'a>, kind: TokenKind) -> Token<'a> {
        let old_consumed_bytes = self.consumed_bytes;
        let new_consumed_bytes = old_consumed_bytes + parsed.consumed;

        self.string = parsed.rest;
        self.consumed_bytes = new_consumed_bytes;

        Token {
            at: old_consumed_bytes..new_consumed_bytes,
            lexeme: parsed.lexeme,
            kind,
        }
    }

    fn advance_and_make_invalid_token(&mut self, parsed: Parsed<'a>) -> InvalidToken<'a> {
        let old_consumed_bytes = self.consumed_bytes;
        let new_consumed_bytes = old_consumed_bytes + parsed.consumed;

        self.string = parsed.rest;
        self.consumed_bytes = new_consumed_bytes;

        InvalidToken {
            at: old_consumed_bytes..new_consumed_bytes,
            lexeme: parsed.lexeme,
        }
    }
}

macro_rules! make_lexer_sequence {
    (
        $self:ident, $string:ident;
        $first_function:ident $(($first_ch:expr))?, $first_kind:expr;
        $($function:ident $(($ch:expr))?, $kind:expr;)*
    ) => {
        if let Some(parsed) = $first_function($string, $($first_ch)?) {
            Ok($self.advance_and_make_token(parsed, $first_kind))
        }
        $(
            else if let Some(parsed) = $function($string, $($ch)?) {
                Ok($self.advance_and_make_token(parsed, $kind))
            }
        )*
        else {
            Err($self.advance_and_make_invalid_token(invalid($string)))
        }
    };
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, InvalidToken<'a>>;

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
            single_char('|'), TokenKind::Pipe;
            single_char('('), TokenKind::OpenParen;
            single_char(')'), TokenKind::CloseParen;
            identifier, TokenKind::Identifier;
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

    Some(Parsed::new(num, rest, consumed))
}

fn d_token(string: &str) -> Option<Parsed> {
    if string.starts_with("d") || string.starts_with("D") {
        Some(Parsed::new(&string[..1], &string[1..], 1))
    } else {
        None
    }
}

fn single_char(string: &str, ch: char) -> Option<Parsed> {
    if string.starts_with(ch) {
        Some(Parsed::new(&string[..1], &string[1..], 1))
    } else {
        None
    }
}

fn identifier(string: &str) -> Option<Parsed> {
    let (last_alpha, last_char) = string
        .char_indices()
        .take_while(|(_, c)| c.is_alphabetic())
        .last()?;

    let width = last_char.len_utf8();
    let alpha = &string[..last_alpha + width];
    let rest = &string[last_alpha + width..];
    let consumed = last_alpha + width;

    Some(Parsed::new(alpha, rest, consumed))
}

fn invalid(string: &str) -> Parsed {
    let (last_index, last_char) = string
        .char_indices()
        .take_while(|(_, c)| !c.is_whitespace())
        .last()
        .expect("Attempted to construct an invalid token with zero characters");
    // This `expect` will only ever fail if `string` begins with whitespace or is empty.
    // `string` may never begin with whitespace, because whitespace is skipped before attempting token recognition
    // `string` may never be empty, because in that case the lexer returns `None` before attempting token recognition

    let width = last_char.len_utf8();
    let index = &string[..last_index + width];
    let rest = &string[last_index + width..];
    let consumed = last_index + width;

    Parsed::new(index, rest, consumed)
}

/// The successful result of a parsing operation
struct Parsed<'a> {
    lexeme: &'a str,
    rest: &'a str,
    consumed: usize,
}

impl<'a> Parsed<'a> {
    fn new(lexeme: &'a str, rest: &'a str, consumed: usize) -> Self {
        Parsed {
            lexeme,
            rest,
            consumed,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    at: Range<usize>,
    lexeme: &'a str,
    kind: TokenKind,
}
/// An invalid token: the result of attempting to lex invalid input
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidToken<'a> {
    at: Range<usize>,
    lexeme: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Integer,
    D,
    Plus,
    Minus,
    Star,
    Slash,
    Pipe,
    OpenParen,
    CloseParen,
    Identifier,
}

#[cfg(test)]
mod test {
    use super::*;

    fn token(at: Range<usize>, lexeme: &str, kind: TokenKind) -> Token {
        Token { at, lexeme, kind }
    }

    fn invalid_token(at: Range<usize>, lexeme: &str) -> InvalidToken {
        InvalidToken { at, lexeme }
    }

    #[test]
    fn one_integer() {
        let lexer = Lexer::new("123");
        let expected = vec![token(0..3, "123", TokenKind::Integer)];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
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

    #[test]
    fn d_tokens() {
        let lexer = Lexer::new("dD");
        let expected = vec![
            token(0..1, "d", TokenKind::D),
            token(1..2, "D", TokenKind::D),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn dice_expression() {
        let lexer = Lexer::new("15d7");
        let expected = vec![
            token(0..2, "15", TokenKind::Integer),
            token(2..3, "d", TokenKind::D),
            token(3..4, "7", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
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
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
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
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn negative_number() {
        let lexer = Lexer::new("-13");
        let expected = vec![
            token(0..1, "-", TokenKind::Minus),
            token(1..3, "13", TokenKind::Integer),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn pipes_and_parens() {
        let lexer = Lexer::new("(|)");
        let expected = vec![
            token(0..1, "(", TokenKind::OpenParen),
            token(1..2, "|", TokenKind::Pipe),
            token(2..3, ")", TokenKind::CloseParen),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn identifier() {
        let lexer = Lexer::new("ident");
        let expected = vec![token(0..5, "ident", TokenKind::Identifier)];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn simple_dice_expression() {
        let lexer = Lexer::new("repeat (6d6 + 3) 5 | top 3 | sum");
        let expected = vec![
            token(0..6, "repeat", TokenKind::Identifier),
            token(7..8, "(", TokenKind::OpenParen),
            token(8..9, "6", TokenKind::Integer),
            token(9..10, "d", TokenKind::D),
            token(10..11, "6", TokenKind::Integer),
            token(12..13, "+", TokenKind::Plus),
            token(14..15, "3", TokenKind::Integer),
            token(15..16, ")", TokenKind::CloseParen),
            token(17..18, "5", TokenKind::Integer),
            token(19..20, "|", TokenKind::Pipe),
            token(21..24, "top", TokenKind::Identifier),
            token(25..26, "3", TokenKind::Integer),
            token(27..28, "|", TokenKind::Pipe),
            token(29..32, "sum", TokenKind::Identifier),
        ];
        assert!(lexer.eq(expected.into_iter().map(|x| Ok(x))));
    }

    #[test]
    fn with_invalid_token() {
        let lexer = Lexer::new("123 @#[§ +");
        let expected = vec![
            Ok(token(0..3, "123", TokenKind::Integer)),
            Err(invalid_token(4..9, "@#[§")),
            Ok(token(10..11, "+", TokenKind::Plus)),
        ];
        assert!(lexer.eq(expected.into_iter()));
    }
}
