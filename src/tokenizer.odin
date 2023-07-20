package purrloin

import "core:fmt"
import "core:strings"
import "core:io"
import "core:testing"
import "core:unicode"
import "core:unicode/utf8"

// src/Language/PureScript/CST/Lexer.hs
// Is this enough? Do I need big-int?
TokInt :: distinct i64
TokNumber :: distinct f64
TokRawString :: distinct string
TokString :: distinct string
TokChar :: distinct rune
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

TokLeftParen :: struct {}
TokRightParen :: struct {}
TokLeftBrace :: struct {}
TokRightBrace :: struct {}
TokLeftSquare :: struct {}
TokRightSquare :: struct {}
TokLeftArrow :: struct {}
TokRightArrow :: struct {}
TokRightFatArrow :: struct {}
TokDoubleColon :: struct {}
TokForall :: struct {}
TokEquals :: struct {}
TokPipe :: struct {}
TokTick :: struct {}
TokDot :: struct {}
TokComma :: struct {}
TokUnderscore :: struct {}
TokBackslash :: struct {}

// Operators are different in different contexts - I didn't really want to
// encode this in the tokenizer, so I didn't. This means there is more logic
// inside the parser but that is fine since things like operator precedence
// cannot be inferred but has to be parsed from the programitself.  
Token :: union {
	TokLeftParen,
	TokRightParen,
	TokLeftBrace,
	TokRightBrace,
	TokLeftSquare,
	TokRightSquare,
	TokLeftArrow,
	TokRightArrow,
	TokRightFatArrow,
	TokDoubleColon,
	TokForall,
	TokEquals,
	TokPipe,
	TokTick,
	TokDot,
	TokComma,
	TokUnderscore,
	TokBackslash,
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
	TokLayoutStart,
	TokLayoutSep,
	TokLayoutEnd,
	TokEof,
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


// ========== BEGIN EXPORTS ==========

// Takes a string and creates a tokenizer, the Tokenizer is lazy and generates
// the tokens as they are querried (hopefully this makes it fast...)
tokenize_from_string :: proc(s: string) -> Tokenizer {
	return Tokenizer{0, 0, strings.Reader{s, 0, -1}}
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

tokenizer_eat_while :: proc(tokenizer: ^Tokenizer, filter: proc(_: rune) -> bool) -> i64 {
	for {
		ri := tokenizer_peek_rune(tokenizer)
		if ri != 0 && filter(ri) do tokenizer_read_rune(tokenizer)
		else do break
	}
	return tokenizer_start(tokenizer)
}

tokenizer_peek_runes :: proc(tokenizer: ^Tokenizer, runes: ^[$B]rune) {
	reader_copy := tokenizer.reader
	for i in 0 ..= (B - 1) {
		ri, ra_size, err := strings.reader_read_rune(&reader_copy)
		if err != .None do ri = 0
		runes[i] = ri
	}
}

tokenizer_peek_rune :: proc(tokenizer: ^Tokenizer) -> rune {
	reader_copy := tokenizer.reader
	ri, ra_size, err := strings.reader_read_rune(&reader_copy)
	if err != .None do ri = 0
	return ri
}


Tokenizer :: struct {
	line:   u64,
	column: u64,
	reader: strings.Reader,
}

tokenizer_start :: proc(tokenizer: ^Tokenizer) -> i64 {
	return tokenizer.reader.i
}

tokenizer_next :: proc(tokenizer: ^Tokenizer) -> Token {
	start := tokenizer_start(tokenizer)
	ra := tokenizer_read_rune(tokenizer)

	switch ra {
	// Whitespace
	case ' ':
		return tokenizer_next(tokenizer)
	// Unicode Stuff
	case '∷':
		return or_operator(tokenizer, TokDoubleColon{}, 0, ra, start)
	case '←':
		return or_operator(tokenizer, TokLeftArrow{}, 0, ra, start)
	case '→':
		return or_operator(tokenizer, TokRightArrow{}, 0, ra, start)
	case '⇒':
		return or_operator(tokenizer, TokRightFatArrow{}, 0, ra, start)
	case '∀':
		return or_operator(tokenizer, TokForall{}, 0, ra, start)
	// ASCII
	case '(':
		// This is wrong
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
	case '`':
		return TokTick{}
	case ',':
		return TokComma{}
	case '|':
		return or_operator(tokenizer, TokPipe{}, 0, ra, start)
	case '.':
		return or_operator(tokenizer, TokDot{}, 0, ra, start)
	case '\\':
		return or_operator(tokenizer, TokBackslash{}, 0, ra, start)
	case '<':
		return or_operator(tokenizer, TokLeftArrow{}, '-', ra, start)
	case '-':
		return or_operator(tokenizer, TokRightArrow{}, '>', ra, start)
	// Needs more love
	case '=':
		{
			rs: [2]rune
			tokenizer_peek_runes(tokenizer, &rs)
			if rs[0] == '>' {
				if rune_is_symbol(rs[1]) {
					return tokenizer_operator(tokenizer, ra, start)
				} else {
					assert(tokenizer_read_rune(tokenizer) == rs[0])
					return TokRightFatArrow{}
				}
			} else {
				if rune_is_symbol(rs[0]) {
					return tokenizer_operator(tokenizer, ra, start)
				} else {
					return TokEquals{}
				}
			}
			assert(false)
		}
	case ':':
		{
			rs: [2]rune
			tokenizer_peek_runes(tokenizer, &rs)
			if rs[0] == ':' {
				if rune_is_symbol(rs[1]) {
					return tokenizer_operator(tokenizer, ra, start)
				} else {
					assert(tokenizer_read_rune(tokenizer) == rs[0])
					return TokDoubleColon{}
				}
			} else {
				return tokenizer_operator(tokenizer, ra, start)
			}
			assert(false)
		}
	case '?':
		return tokenizer_hole(tokenizer, ra, start)
	case '\'':
		return tokenizer_char(tokenizer, ra, start)
	case '"':
		return tokenizer_string(tokenizer, ra, start)
	}
	if rune_is_digit(ra) do return tokenizer_int_or_number(tokenizer, ra, start)
	if rune_is_upper(ra) do return tokenizer_upper(tokenizer, ra, start)
	// if rune_is_lower(ra) do return tokenizer_lower(tokenizer, ra, start)
	// if rune_is_ident_start(ra) do return tokenizer_lower(tokenizer, ra, start)
	if rune_is_symbol(ra) do return tokenizer_operator(tokenizer, ra, start)
	// This is an error!
	// assert(false)
	return nil
}

or_operator :: proc(
	tokenizer: ^Tokenizer,
	token_if: Token,
	expect: rune,
	ra: rune,
	start: i64,
) -> Token {
	rb := tokenizer_peek_rune(tokenizer)
	if rb == expect {
		assert(tokenizer_read_rune(tokenizer) == rb)
		return token_if
	}
	if expect == 0 && !rune_is_symbol(rb) {
		return token_if
	}
	return tokenizer_operator(tokenizer, ra, start)
}

tokenizer_hole :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
	end := tokenizer_eat_while(tokenizer, rune_is_ident)
	if end == start + 1 do return tokenizer_operator(tokenizer, ra, start)
	return TokHole(tokenizer_slice_as_string(tokenizer, start, end))
}

tokenizer_char :: proc(tokenizer: ^Tokenizer, _: rune, start: i64) -> Token {
	rb := tokenizer_read_rune(tokenizer)
	char: rune
	if rb == '\\' {
		char = tokenizer_escape(tokenizer)
	} else {
		char = rb
	}
	close := tokenizer_read_rune(tokenizer)
	if close == '\'' {
		end := tokenizer_start(tokenizer)
		return TokChar(char)
	} else {
		return nil
	}
}

tokenizer_escape :: proc(tokenizer: ^Tokenizer) -> rune {
	rb := tokenizer_read_rune(tokenizer)
	switch rb {
	case 't':
		return '\t'
	case 'r':
		return '\r'
	case 'n':
		return '\n'
	case '"':
		return '\"'
	case '\'':
		return '\''
	case '\\':
		return '\\'
	case 'x':
		{
			mx: [6]rune
			tokenizer_peek_runes(tokenizer, &mx)
			char: rune = 0
			for r, i in mx {
				if rune_is_hex_digit(r) {
					tokenizer_read_rune(tokenizer)
					char = char * 16 + rune(rune_to_digit(r))
				} else {
					break
				}
			}
			return char
		}

	}
	return 0
}

tokenizer_int_or_number :: proc(tokenizer: ^Tokenizer, prev: rune, start: i64) -> Token {
	return nil
}

tokenizer_string :: proc(tokenizer: ^Tokenizer, _: rune, start: i64) -> Token {
	rn: [7]rune
	tokenizer_peek_runes(tokenizer, &rn)
	num_quotes_after_first: int
	for r, i in rn {
		num_quotes_after_first = i
		if r == '"' do continue
		else do break
	}

	switch num_quotes_after_first {
	case 0:
		// Normal string
		st: [dynamic]rune
		defer delete(st)
		loop: for {
			c := tokenizer_read_rune(tokenizer)
			switch c {
			case '\r', '\n':
				// Error! Not supported in strings!
				return nil
			case '"':
				break loop
			case '\\':
				before := tokenizer_start(tokenizer)
				after := tokenizer_eat_while(tokenizer, rune_is_string_gap)
				if after - before == 0 {
					append(&st, tokenizer_escape(tokenizer))
				} else {
					d := tokenizer_read_rune(tokenizer)
					switch d {
					case '"':
						break loop
					case '\\':
						continue loop
					case:
						// Error! Invalid char in gap!
						return nil
					}
				}
			case:
				append(&st, c)
			}

		}

		return TokString(utf8.runes_to_string(st[:]))
	case 1:
		// Normal empty string
		return TokString("")
	case 2, 3, 4:
		for _ in 1 ..= 2 {
			tokenizer_read_rune(tokenizer)
		}
		for {
			tokenizer_eat_while(tokenizer, rune_is_not_quote)
			rn: [5]rune
			tokenizer_peek_runes(tokenizer, &rn)
			num_quotes: int
			for r, i in rn {
				num_quotes = i
				if r == '"' do continue
				else do break
			}

			for _ in 1 ..= num_quotes {
				tokenizer_read_rune(tokenizer)
			}
			switch num_quotes {
			case 0:
				// EoF
				return nil
			case 1, 2:
				continue
			case 3, 4, 5:
				end := tokenizer_start(tokenizer)
				return TokRawString(tokenizer_slice_as_string(tokenizer, start + 3, end - 3))
			}
		}

	case 5:
		// Raw string with quotes
		for _ in 1 ..= 5 {
			tokenizer_read_rune(tokenizer)
		}
		return TokRawString("")
	case 6:
		for _ in 1 ..= 5 {
			tokenizer_read_rune(tokenizer)
		}
		return TokRawString("\"")
	case 7:
		for _ in 1 ..= 5 {
			tokenizer_read_rune(tokenizer)
		}
		return TokRawString("\"\"")

	}

	return nil
}

tokenizer_upper :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
	return nil
}

tokenizer_lower :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
	return nil
}

// TODO: Take in Qualified
tokenizer_operator :: proc(tokenizer: ^Tokenizer, _: rune, start: i64) -> Token {
	end := tokenizer_eat_while(tokenizer, rune_is_symbol)
	return TokOperator(tokenizer_slice_as_string(tokenizer, start, end))
}

rune_to_digit :: proc(r: rune) -> i64 {
	switch r {
	case '0' ..= '9':
		return i64(r - '0')
	case 'a' ..= 'f':
		return 10 + i64(r - 'a')
	case 'A' ..= 'F':
		return 10 + i64(r - 'A')
	}
	assert(false)
	return 0
}

rune_is_ident :: proc(r: rune) -> bool {
	return unicode.is_alpha(r) || unicode.is_number(r) || r == '_' || r == '\''
}

rune_is_white_space :: unicode.is_white_space
rune_is_digit :: unicode.is_digit
rune_is_lower :: unicode.is_lower
rune_is_upper :: unicode.is_upper

rune_is_not_quote :: proc(r: rune) -> bool {return r != '\"'}

rune_is_string_gap :: proc(r: rune) -> bool {return r == ' ' || r == '\r' || r == '\n'}

rune_is_hex_digit :: proc(r: rune) -> bool {
	if rune_is_digit(r) do return true
	switch r {
	case 'a' ..= 'f', 'A' ..= 'F':
		return true
	}
	return false
}

rune_is_symbol :: proc(r: rune) -> bool {
	// All the ASCII cases are hardcoded
	switch r {
	case 0:
		return false
	case '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~':
		return true
	}
	// If it's unicode and a symbol - it's a valid operator-symbol (this is kinda mad)
	return (!rune_is_ascii(r)) && unicode.is_symbol(r)
}

rune_is_ascii :: proc(r: rune) -> bool {
	return r <= unicode.MAX_ASCII
}

tokenizer_slice_as_string :: proc(t: ^Tokenizer, lo, hi: i64) -> string {
	return transmute(string)(transmute([]u8)t.reader.s)[lo:hi]
}

when ODIN_TEST {
	expect_tokens :: proc(t: ^testing.T, tokens: []Token, str: string, loc := #caller_location) {
		tokenizer := tokenize_from_string(str)
		errors := false
		for expected, i in tokens {
			value := tokenizer_next(&tokenizer)
			if value != expected {
				errors = true
				testing.errorf(
					t,
					"Expected %v-th token to be '%v' but got '%v'",
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

		if errors {
			testing.errorf(t, "Got errors for string: %v", str, loc = loc)
		}
	}

	// 	@(test)
	// 	parse_int_0 :: proc(t: ^testing.T) {
	// 		expect_tokens(t, []Token{TokInt(1)}, "1")
	// 	}
	// 
	// 	@(test)
	// 	parse_int_1 :: proc(t: ^testing.T) {
	// 		expect_tokens(t, []Token{TokInt(123)}, "123")
	// 	}
	// 
	// 	@(test)
	// 	parse_int_2 :: proc(t: ^testing.T) {
	// 		expect_tokens(t, []Token{TokInt(10)}, "0000010")
	// 	}

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
			[]Token{TokRightArrow{}, TokLeftArrow{}, TokOperator("<="), TokRightFatArrow{}},
			"-> <- <= =>",
		)
	}

	@(test)
	parse_arrow_like_operators :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{TokOperator("-"), TokOperator("<"), TokEquals{}, TokPipe{}},
			" - < = | ",
		)
	}

	@(test)
	parse_colons :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{TokOperator(":"), TokDoubleColon{}, TokDoubleColon{}, TokOperator(":")},
			": :: :::",
		)
	}

	@(test)
	parse_dot_like :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokDot{}, TokTick{}, TokComma{}, TokBackslash{}}, ".`,\\")
	}

	@(test)
	parse_char_simple :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokChar('a'),
				TokChar('b'),
				TokChar('c'),
				TokChar('#'),
				TokChar('?'),
				TokChar('?'),
				TokChar(0),
			},
			"'a' 'b' 'c' '\\x23' '\\x3f' '\\x3F' '\\x'",
		)
	}

	@(test)
	parse_string_simple :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokString("abc"),
				TokString("a \"string\""),
				TokRawString("aabbcc"),
				TokRawString(""),
				TokString("#$%"),
			},
			"\"abc\" \"a \\\"string\\\"\" \"\"\"aabbcc\"\"\" \"\"\"\"\"\" \"\\x23\\x24\\    \\\\x25\"",
		)
	}
}

