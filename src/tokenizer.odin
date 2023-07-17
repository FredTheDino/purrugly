package purrloin

import "core:fmt"
import "core:strings"
import "core:io"
import "core:testing"
import "core:unicode"

// src/Language/PureScript/CST/Lexer.hs
// Is this enough? Do I need big-int?
TokInt :: struct {
	as_int:  i64,
	as_text: string,
}
TokNumber :: struct {
	as_number: f64,
	as_text:   string,
}
TokRawString :: distinct string
TokString :: distinct string
TokChar :: distinct string
TokHole :: distinct string
TokUpper :: distinct string
TokLower :: distinct string
TokSymbol :: distinct string
TokOperator :: distinct string
TokLineComment :: distinct string
TokBlockComment :: distinct string
TokEof :: struct {}
TokLayoutStart :: struct {}
TokLayoutSep :: struct {}
TokLayoutEnd :: struct {}

// Operators are different in different contexts - I didn't really want to
// encode this in the tokenizer, so I didn't. This means there is more logic
// inside the parser but that is fine since things like operator precedence
// cannot be inferred but has to be parsed from the programitself.  
Token :: union {
	TokInt,
	TokNumber,
	TokRawString,
	TokString,
	TokChar,
	TokHole,
	TokUpper,
	TokLower,
	TokSymbol,
	TokOperator,
}

// Lifted straight from the PureScript lexer
/*
data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow !SourceStyle
  | TokRightArrow !SourceStyle
  | TokRightFatArrow !SourceStyle
  | TokDoubleColon !SourceStyle
  | TokForall !SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokLowerName ![Text] !Text
  | TokUpperName ![Text] !Text
  | TokOperator ![Text] !Text
  | TokSymbolName ![Text] !Text
  | TokSymbolArr !SourceStyle
  | TokHole !Text
  | TokChar !Text !Char
  | TokString !Text !PSString
  | TokRawString !Text
  | TokInt !Text !Integer
  | TokNumber !Text !Double
  | TokLayoutStart
  | TokLayoutSep
  | TokLayoutEnd
  | TokEof
  deriving (Show, Eq, Ord, Generic)
*/


// BEGIN EXPORTS

// Takes a string and creates a tokenizer, the Tokenizer is lazy and generates
// the tokens as they are querried (hopefully this makes it fast...)
tokenize_from_string :: proc(s: string) -> Tokenizer {
	return Tokenizer{ 0, 0, strings.Reader{s, 0, -1} }
}

// Read the next rune - cannot be reversed so tread carefully.
tokenizer_read_rune :: proc(tokenizer: ^Tokenizer) -> rune {
	ra, ra_size, err := strings.reader_read_rune(&tokenizer.reader)
  if err != .None do return 0

  if ra == '\n' {
    tokenizer.line += 1
    tokenizer.column = 0
  }
  return ra
}
// END EXPORTS

Tokenizer :: struct {
  line: u64,
  column: u64,
  reader: strings.Reader
}


tokenizer_start :: proc(tokenizer: ^Tokenizer) -> i64 {
  return tokenizer.reader.i
}

tokenizer_next :: proc(tokenizer: ^Tokenizer) -> Token {
  start := tokenizer_start(tokenizer)
	ra := tokenizer_read_rune(tokenizer)

  if rune_is_digit(ra) {
    return tokenizer_int_or_number(tokenizer, ra, start)
  } else if rune_is_upper(ra) {
    return tokenizer_upper(tokenizer, ra, start)
  } else if rune_is_lower(ra) {
    return tokenizer_lower(tokenizer, ra, start)
  }
  return nil
}

tokenizer_int_or_number :: proc(tokenizer: ^Tokenizer, prev: rune, start: i64) -> Token {
  return nil
}

tokenizer_upper :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
  return nil
}

tokenizer_lower :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
  return nil
}

rune_to_digit :: proc(r: rune) -> i64 {
	return i64(r - '0')
}

rune_is_white_space :: unicode.is_white_space
rune_is_digit :: unicode.is_digit
rune_is_lower :: unicode.is_lower
rune_is_upper :: unicode.is_upper

rune_is_symbol :: proc(r: rune) -> bool {
  // All the ASCII cases are hardcoded
  switch r {
    case '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~':
      return true
  }
  // If it's unicode and a symbol - it's a valid operator-symbol (this is kinda mad)
  return (!rune_is_ascii(r)) && unicode.is_symbol(r)
}

rune_is_ascii :: proc(r: rune) -> bool {
	return r <= unicode.MAX_ASCII
}

sub_string :: proc(s: string, lo, hi: i64) -> string {
	return transmute(string)(transmute([]u8)s)[lo:hi]
}


// 
// isReservedSymbolError :: ParserErrorType -> Bool
// isReservedSymbolError = (== ErrReservedSymbol)
// 
// isReservedSymbol :: Text -> Bool
// isReservedSymbol = flip elem symbols
//   where
//   symbols =
//     [ "::"
//     , "∷"
//     , "<-"
//     , "←"
//     , "->"
//     , "→"
//     , "=>"
//     , "⇒"
//     , "∀"
//     , "|"
//     , "."
//     , "\\"
//     , "="
//     ]
// 
// isIdentStart :: Char -> Bool
// isIdentStart c = Char.isLower c || c == '_'
// 
// isIdentChar :: Char -> Bool
// isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''
// 
// isNumberChar :: Char -> Bool
// isNumberChar c = Char.isDigit c || c == '_'
// 
// isNormalStringChar :: Char -> Bool
// isNormalStringChar c = c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'
// 
// isStringGapChar :: Char -> Bool
// isStringGapChar c = c == ' ' || c == '\r' || c == '\n'
// 
// isLineFeed :: Char -> Bool
// isLineFeed c = c == '\r' || c == '\n'


// tokenizer_int :: proc(tokenizer: ^Tokenizer, current: i64, start: i64) -> TokInt {
// 	current := current
// 	loop: for {
// 		rr, size, err := strings.reader_read_rune(tokenizer)
// 		if err != .None do break
// 		switch rr {
// 		case '0' ..= '9':
// 			current = 10 * current + rune_to_digit(rr)
// 		case:
// 			strings.reader_unread_rune(tokenizer)
// 			break loop // Otherwise it breaks out of the for-loop
// 		}
// 	}
// 	return TokInt{as_int = current, as_text = sub_string(tokenizer.s, start, tokenizer.i)}
// }

when ODIN_TEST {
	expect_tokens :: proc(t: ^testing.T, tokens: []Token, str: string, loc := #caller_location) {
		tokenizer := tokenize_from_string(str)
		for expected, i in tokens {
			value := tokenizer_next(&tokenizer)
			if value != expected {
				testing.errorf(
					t,
					"Expected %v-th token to be %v but got %v",
					i,
					expected,
					value,
					loc = loc,
				)
			}
		}
		last := tokenizer_next(&tokenizer)
		if last != nil {
			testing.errorf(t, "Got more tokens after the last one: %v", last, loc = loc)
		}
	}

	@(test)
	parse_int_0 :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokInt(1)}, "1")
	}

	@(test)
	parse_int_1 :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokInt(123)}, "123")
	}

	@(test)
	parse_int_2 :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokInt(10)}, "0000010")
	}

	//
	@(test)
	parse_different_brackets :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokLeftParen{},
				TokRightParen{},
				TokLeftBrace{},
				TokRightBrace{},
				TokLeftSquare{},
				TokRightSquare{},
			},
			"(){}[]",
		)
	}

	@(test)
	parse_arrows :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{TokRightArrow{}, TokLeftArrow{}, TokLeftFatArrow{}, TokRightFatArrow{}},
			"-> <- <= =>",
		)
	}

	@(test)
	parse_arrow_like_operators :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{TokOperator('-'), TokOperator('<'), TokEquals{}, TokPipe{}},
			"- < = |",
		)
	}

	@(test)
	parse_colons :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokOperator(':'), TokDoubleColor{}}, ": ::")
	}

	@(test)
	parse_dot_like :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{TokDot{}, TokTick{}, TokComma{}, TokUnderscore{}, TokBackslash{}},
			".`,_\\",
		)
	}

}
