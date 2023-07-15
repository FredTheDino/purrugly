package purrloin

import "core:fmt"
import "core:strings"
import "core:io"
import "core:testing"

// Is this enough? Do I need big-int?
TokInt :: distinct i64
TokNumber :: distinct f64

TokLeftParen :: struct {}
TokRightParen :: struct {}

TokLeftBrace :: struct {}
TokRightBrace :: struct {}

TokLeftSquare :: struct {}
TokRightSquare :: struct {}

TokLeftArrow :: struct {}
TokRightArrow :: struct {}

TokLeftFatArrow :: struct {}
TokRightFatArrow :: struct {}

TokEquals :: struct {}
TokDoubleColor :: struct {}
TokPipe :: struct {}

TokTick :: struct {}
TokComma :: struct {}
TokDot :: struct {}
TokUnderscore :: struct {}
TokBackslash :: struct {}

TokOperator :: distinct rune

Token :: union {
	TokLeftParen,
	TokRightParen,
	//
	TokLeftBrace,
	TokRightBrace,
	//
	TokLeftSquare,
	TokRightSquare,
	//
	TokLeftArrow,
	TokRightArrow,
	//
	TokLeftFatArrow,
	TokRightFatArrow,
  //
  TokEquals,
  TokDoubleColor,
  TokPipe,
  TokTick ,
  TokComma ,
  TokDot ,
  TokUnderscore ,
  TokBackslash,
	//
	TokOperator,
	//
	TokInt,
	TokNumber,
}

Tokenizer :: struct {
	reader: strings.Reader,
}

tokenize_from_string :: proc(s: string) -> Tokenizer {
	r := strings.Reader{s, 0, -1}
	return Tokenizer{r}
}

tokenizer_next :: proc(tokenizer: ^Tokenizer) -> Token {
	for {
		ra, ra_size, err := strings.reader_read_rune(&tokenizer.reader)
		if err != .None do return nil
		switch ra {
		case '(':
			return TokLeftParen{}
		case ')':
			return TokRightParen{}

		case '{':
			return TokLeftBrace{}
		case '}':
			return TokRightBrace{}

		case '[':
			return TokLeftSquare{}
		case ']':
			return TokRightSquare{}

		case '-':
			rb, rb_size, err := strings.reader_read_rune(&tokenizer.reader)
			if err != .None || rb != '>' {
				strings.reader_unread_rune(&tokenizer.reader)
				return TokOperator(ra)
			} else {
				return TokRightArrow{}
			}
		case '<':
			rb, rb_size, err := strings.reader_read_rune(&tokenizer.reader)
      next_is_valid := rb == '-' || rb == '='
			if err != .None || (!next_is_valid) {
				strings.reader_unread_rune(&tokenizer.reader)
				return TokOperator(ra)
			} else {
				return TokLeftArrow{} if rb == '-' else TokLeftFatArrow{}
			}

		case '=':
			rb, rb_size, err := strings.reader_read_rune(&tokenizer.reader)
			if err != .None || rb != '>' {
				strings.reader_unread_rune(&tokenizer.reader)
				return TokEquals{}
			} else {
				return TokRightFatArrow{}
			}

    case ':':
			rb, rb_size, err := strings.reader_read_rune(&tokenizer.reader)
			if err != .None || rb != ':' {
				strings.reader_unread_rune(&tokenizer.reader)
				return TokOperator(ra)
			} else {
				return TokDoubleColor{}
			}

    case '|':
      return TokPipe{}
    case '`':
      return TokTick{}
    case ',':
      return TokComma{}
    case '.':
      return TokDot{}
    case '_':
      return TokUnderscore{}
    case '@':
      return TokOperator(ra)
    case '\\':
      return TokBackslash{}

		case '0' ..= '9':
			return tokenizer_int(tokenizer, rune_to_digit(ra))

		// Skip all whitespace
		case ' ':
			continue
		// Error - unknown token
		case:
			return nil
		}
	}

}

rune_to_digit :: proc(r: rune) -> i64 {
	return i64(r - '0')
}

tokenizer_int :: proc(tokenizer: ^Tokenizer, current: i64) -> TokInt {
	current := current
	loop: for {
		rr, size, err := strings.reader_read_rune(&tokenizer.reader)
		if err != .None do break
		switch rr {
		case '0' ..= '9':
			current = 10 * current + rune_to_digit(rr)
		case:
			strings.reader_unread_rune(&tokenizer.reader)
			break loop // Otherwise it breaks out of the for-loop
		}
	}
	return TokInt(current)
}

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
		expect_tokens(t, []Token{TokRightArrow{}, TokLeftArrow{}, TokLeftFatArrow{}, TokRightFatArrow{}}, "-> <- <= =>")
	}

	@(test)
	parse_arrow_like_operators :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokOperator('-'), TokOperator('<'), TokEquals{}, TokPipe{}}, "- < = |")
	}

	@(test)
	parse_colons :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokOperator(':'), TokDoubleColor{}}, ": ::")
	}

	@(test)
	parse_dot_like :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokDot{}, TokTick{}, TokComma{}, TokUnderscore{}, TokBackslash{}}, ".`,_\\")
	}

}
