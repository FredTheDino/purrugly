// A complete lexer for the PureScript language, heavily inspired by the one
// from the mainline compiler.
package purrloin

import "core:fmt"
import "core:strings"
import "core:io"
import "core:testing"
import "core:unicode"
import "core:unicode/utf8"
import "core:slice"
import "core:math"

// src/Language/PureScript/CST/Lexer.hs
// Is this enough? Do I need big-int?
TokInt :: distinct i64
TokNumber :: distinct f64
TokRawString :: distinct string
TokString :: distinct string
TokChar :: distinct rune
TokHole :: distinct string
TokUpperName :: struct {
	qual: []string,
	name: string,
}
TokLowerName :: struct {
	qual: []string,
	name: string,
}
TokSymbolName :: struct {
	qual: []string,
	name: string,
}
TokOperator :: struct {
	qual: []string,
	name: string,
}
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
TokSymbolArr :: struct {}

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
	TokSymbolArr,
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
	TokUpperName,
	TokLowerName,
	TokSymbolName,
	TokOperator,
	TokLayoutStart,
	TokLayoutSep,
	TokLayoutEnd,
	TokEof,
}

SpannedToken :: struct {
	token:  Token,
	raw:    string,
	start:  i64,
	end:    i64,
	line:   u64,
	column: u64,
}

// Takes a string and creates a tokenizer, the Tokenizer is lazy and generates
// the tokens as they are querried (hopefully this makes it fast...)
tokenize_from_string :: proc(s: string) -> Tokenizer {
	return Tokenizer{0, 0, strings.Reader{s, 0, -1}}
}

tokenizer_eat :: proc(tokenizer: ^Tokenizer) -> rune {
	ra, ra_size, err := strings.reader_read_rune(&tokenizer.reader)
	if err != .None do return 0

	if ra == '\n' {
		tokenizer.line += 1
		tokenizer.column = 0
	}
	return ra
}

tokenizer_while :: proc(tokenizer: ^Tokenizer, filter: proc(_: rune) -> bool) -> i64 {
	for {
		ri := tokenizer_peek(tokenizer)
		if ri != 0 && filter(ri) do tokenizer_eat(tokenizer)
		else do break
	}
	return tokenizer_at(tokenizer)
}

tokenizer_peek_runes :: proc(tokenizer: ^Tokenizer, runes: ^[$B]rune) {
	reader_copy := tokenizer.reader
	for i in 0 ..< B {
		ri, ra_size, err := strings.reader_read_rune(&reader_copy)
		if err != .None do ri = 0
		runes[i] = ri
	}
}

tokenizer_peek :: proc(tokenizer: ^Tokenizer) -> rune {
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

tokenizer_at :: proc(tokenizer: ^Tokenizer) -> i64 {
	return tokenizer.reader.i
}

tokenizer_skip :: proc(tokenizer: ^Tokenizer) {
	tokenizer_while(tokenizer, rune_is_white_space)
}

tokenizer_next_spanned :: proc(tokenizer: ^Tokenizer) -> SpannedToken {
	tokenizer_skip(tokenizer)
	line := tokenizer.line
	column := tokenizer.column
	start := tokenizer_at(tokenizer)
	token := tokenizer_next(tokenizer)
	end := tokenizer_at(tokenizer)
	raw := tokenizer_slice(tokenizer, start, end)

	return SpannedToken{token, raw, start, end, line, column}
}

tokenizer_next :: proc(tokenizer: ^Tokenizer) -> Token {
	qual: [dynamic]string
	defer delete(qual)

	tokenizer_skip(tokenizer)
	start := tokenizer_at(tokenizer)
	ra := tokenizer_eat(tokenizer)

	switch ra {
	// Whitespace
	case ' ', '\n', '\r':
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
		return tokenizer_left_paren(tokenizer)
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
					return tokenizer_operator(tokenizer, &qual, ra, start)
				} else {
					assert(tokenizer_eat(tokenizer) == rs[0])
					return TokRightFatArrow{}
				}
			} else {
				if rune_is_symbol(rs[0]) {
					return tokenizer_operator(tokenizer, &qual, ra, start)
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
					return tokenizer_operator(tokenizer, &qual, ra, start)
				} else {
					assert(tokenizer_eat(tokenizer) == rs[0])
					return TokDoubleColon{}
				}
			} else {
				return tokenizer_operator(tokenizer, &qual, ra, start)
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
	if rune_is_upper(ra) do return tokenizer_upper(tokenizer, &qual, ra, start)
	if rune_is_ident_start(ra) do return tokenizer_lower(tokenizer, &qual, ra, start)
	if rune_is_symbol(ra) do return tokenizer_operator(tokenizer, &qual, ra, start)
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
	rb := tokenizer_peek(tokenizer)
	if rb == expect {
		assert(tokenizer_eat(tokenizer) == rb)
		return token_if
	}
	if expect == 0 && !rune_is_symbol(rb) {
		return token_if
	}

	qual: [dynamic]string
	defer delete(qual)
	return tokenizer_operator(tokenizer, &qual, ra, start)
}

tokenizer_left_paren :: proc(tokenizer: ^Tokenizer) -> Token {
	reset := tokenizer^
	start := tokenizer_at(tokenizer)
	end := tokenizer_while(tokenizer, rune_is_symbol)
	if start == end {
		return TokLeftParen{}
	}
	symbol := tokenizer_slice(tokenizer, start, end)
	rb := tokenizer_eat(tokenizer)
	if rb != ')' do return nil
	if symbol == "->" || symbol == "→" {
		return TokSymbolArr{}
	}
	if is_reserved_symbol(symbol) do return nil
	tokenizer^ = reset
	return TokLeftParen{}
}

tokenizer_hole :: proc(tokenizer: ^Tokenizer, ra: rune, start: i64) -> Token {
	end := tokenizer_while(tokenizer, rune_is_ident)
	if end == start + 1 {
		qual: [dynamic]string
		defer delete(qual)
		return tokenizer_operator(tokenizer, &qual, ra, start)
	}
	return TokHole(tokenizer_slice(tokenizer, start, end))
}

tokenizer_char :: proc(tokenizer: ^Tokenizer, _: rune, start: i64) -> Token {
	rb := tokenizer_eat(tokenizer)
	char: rune
	if rb == '\\' {
		char = tokenizer_escape(tokenizer)
	} else {
		char = rb
	}
	close := tokenizer_eat(tokenizer)
	if close == '\'' {
		end := tokenizer_at(tokenizer)
		return TokChar(char)
	} else {
		return nil
	}
}

tokenizer_escape :: proc(tokenizer: ^Tokenizer) -> rune {
	rb := tokenizer_eat(tokenizer)
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
					tokenizer_eat(tokenizer)
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
	rb := tokenizer_peek(tokenizer)
	if prev == '0' && rb == 'x' {
		tokenizer_eat(tokenizer)
		return tokenizer_hexadecimal(tokenizer)
	}

	tokenizer_int :: proc(tokenizer: ^Tokenizer, offset: i64 = 0) -> (int_value: int, err: bool) {
		// Include the actual start of the number
		start := tokenizer_at(tokenizer) + offset
		end := tokenizer_while(tokenizer, rune_is_number_digit)
		if start == end {
			// Not a valid int
			return 0, true
		}
		num_digits := 0
		for r in tokenizer_slice(tokenizer, start, end) {
			// TODO: This isn't robust enough for parsing large integers
			assert(rune_is_number_digit(r))
			if r == '_' do continue
			// Err leading zero - not really sure why this is disallowed?
			if r == '0' && int_value == 0 && num_digits > 0 do return 0, true
			num_digits += 1
			int_value = int_value * 10 + int(rune_to_digit(r))
		}
		return int_value, false
	}
	int_value, err := tokenizer_int(tokenizer, offset = -1)
	if err do return nil

	tokenizer_fraction :: proc(
		tokenizer: ^Tokenizer,
	) -> (
		has_fraction: bool,
		exp: int,
		frac_value: int,
	) {
		rb := tokenizer_peek(tokenizer)
		if rb != '.' do return
		tokenizer_eat(tokenizer)

		start := tokenizer_at(tokenizer)
		end := tokenizer_while(tokenizer, rune_is_number_digit)
		if start == end do return
		has_fraction = true
		for r in tokenizer_slice(tokenizer, start, end) {
			// TODO: This isn't robust enough for parsing large integers
			assert(rune_is_number_digit(r))
			if r == '_' do continue
			exp = exp + 1
			frac_value = frac_value * 10 + int(rune_to_digit(r))
		}
		return
	}
	has_fraction, exp, frac_value := tokenizer_fraction(tokenizer)

	tokenizer_exponent :: proc(tokenizer: ^Tokenizer) -> (has_exponent: bool, exponent: int) {
		maybe_e := tokenizer_peek(tokenizer)
		if maybe_e != 'e' do return
		tokenizer_eat(tokenizer)

		maybe_sign := tokenizer_peek(tokenizer)

		is_negative: bool
		switch maybe_sign {
		case '+', '-':
			is_negative = maybe_sign == '-'
			tokenizer_eat(tokenizer)
		case:
			is_negative = false
		}

		int_value, err := tokenizer_int(tokenizer)
		if err do return
		exponent = (-int_value) if is_negative else (int_value)
		has_exponent = true
		return
	}
	has_exponent, exponent := tokenizer_exponent(tokenizer)

	if has_exponent || has_fraction {
		return TokNumber(
			math.pow10(f64(exponent)) *
			(f64(int_value) + (f64(frac_value) * math.pow10(f64(-exp)))),
		)
	} else {
		return TokInt(int_value)
	}
}

tokenizer_hexadecimal :: proc(tokenizer: ^Tokenizer) -> Token {
	start := tokenizer_at(tokenizer)
	end := tokenizer_while(tokenizer, rune_is_hex_digit)
	if start == end {
		// Not a valid hex-digit (Maybe 0 is a sane default value
		return nil
	}
	number := 0
	for r in tokenizer_slice(tokenizer, start, end) {
		// TODO: This isn't robust enough for parsing large integers
		assert(rune_is_hex_digit(r))
		number = number * 16 + int(rune_to_digit(r))
	}
	return TokInt(number)
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
			c := tokenizer_eat(tokenizer)
			switch c {
			case '\r', '\n':
				// Error! Not supported in strings!
				return nil
			case '"':
				break loop
			case '\\':
				before := tokenizer_at(tokenizer)
				after := tokenizer_while(tokenizer, rune_is_string_gap)
				if after - before == 0 {
					append(&st, tokenizer_escape(tokenizer))
				} else {
					d := tokenizer_eat(tokenizer)
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
			tokenizer_eat(tokenizer)
		}
		for {
			tokenizer_while(tokenizer, rune_is_not_quote)
			rn: [5]rune
			tokenizer_peek_runes(tokenizer, &rn)
			num_quotes: int
			for r, i in rn {
				num_quotes = i
				if r == '"' do continue
				else do break
			}

			for _ in 1 ..= num_quotes {
				tokenizer_eat(tokenizer)
			}
			switch num_quotes {
			case 0:
				// EoF
				return nil
			case 1, 2:
				continue
			case 3, 4, 5:
				end := tokenizer_at(tokenizer)
				return TokRawString(tokenizer_slice(tokenizer, start + 3, end - 3))
			}
		}

	case 5:
		// Raw string with quotes
		for _ in 1 ..= 5 {
			tokenizer_eat(tokenizer)
		}
		return TokRawString("")
	case 6:
		for _ in 1 ..= 5 {
			tokenizer_eat(tokenizer)
		}
		return TokRawString("\"")
	case 7:
		for _ in 1 ..= 5 {
			tokenizer_eat(tokenizer)
		}
		return TokRawString("\"\"")

	}

	return nil
}

tokenizer_upper :: proc(
	tokenizer: ^Tokenizer,
	qual: ^[dynamic]string,
	_ra: rune,
	start: i64,
) -> Token {
	end := tokenizer_while(tokenizer, rune_is_ident)
	rr := tokenizer_peek(tokenizer)
	if rr == '.' {
		append(qual, tokenizer_slice(tokenizer, start, end))
		tokenizer_eat(tokenizer)
		inner_start := tokenizer_at(tokenizer)
		inner_ra := tokenizer_eat(tokenizer)
		if inner_ra == '(' do return tokenizer_symbol(tokenizer, qual)
		if rune_is_upper(inner_ra) do return tokenizer_upper(tokenizer, qual, inner_ra, inner_start)
		if rune_is_ident_start(inner_ra) do return tokenizer_lower(tokenizer, qual, inner_ra, inner_start)
		if rune_is_symbol(inner_ra) do return tokenizer_operator(tokenizer, qual, inner_ra, inner_start)
		return nil
	} else {
		qual_copied := slice.clone(qual[:])
		return TokUpperName{qual_copied, tokenizer_slice(tokenizer, start, end)}
	}
}

tokenizer_symbol :: proc(tokenizer: ^Tokenizer, qual: ^[dynamic]string) -> Token {
	start := tokenizer_at(tokenizer)
	end := tokenizer_while(tokenizer, rune_is_symbol)

	ra := tokenizer_peek(tokenizer)
	if ra == ')' {
		tokenizer_eat(tokenizer)
		qual_copied := slice.clone(qual[:])
		name := tokenizer_slice(tokenizer, start, end)
		if is_reserved_symbol(name) do return nil
		else do return TokSymbolName{qual_copied, name}
	}
	return nil
}

tokenizer_lower :: proc(
	tokenizer: ^Tokenizer,
	qual: ^[dynamic]string,
	ra: rune,
	start: i64,
) -> Token {
	end := tokenizer_while(tokenizer, rune_is_ident)
	if ra == '_' {
		if qual == nil {
			return TokUnderscore{}
		} else {
			// Error - '_' is a magical symbol
			return nil
		}
	}

	name := tokenizer_slice(tokenizer, start, end)
	if name == "forall" {
		return TokForall{}
	} else {
		qual_copied := slice.clone(qual[:])
		return TokLowerName{qual_copied, name}
	}
}

// TODO: Take in Qualified
tokenizer_operator :: proc(
	tokenizer: ^Tokenizer,
	qual: ^[dynamic]string,
	_: rune,
	start: i64,
) -> Token {
	end := tokenizer_while(tokenizer, rune_is_symbol)
	qual_copied := slice.clone(qual[:])
	return TokOperator{qual_copied, tokenizer_slice(tokenizer, start, end)}
}

is_reserved_symbol :: proc(s: string) -> bool {
	switch s {
	case "::", "∷", "<-", "←", "->", "→", "=>", "⇒", "∀", "|", ".", "\\", "=":
		return true
	case:
		return false
	}
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

rune_is_ident_start :: proc(r: rune) -> bool {return r == '_' || rune_is_lower(r)}

rune_is_not_quote :: proc(r: rune) -> bool {return r != '\"'}

rune_is_string_gap :: proc(r: rune) -> bool {return r == ' ' || r == '\r' || r == '\n'}

rune_is_number_digit :: proc(r: rune) -> bool {return r == '_' || rune_is_digit(r)}

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

tokenizer_slice :: proc(t: ^Tokenizer, lo, hi: i64) -> string {
	return transmute(string)(transmute([]u8)t.reader.s)[lo:hi]
}

when ODIN_TEST {
	expect_tokens :: proc(t: ^testing.T, tokens: []Token, str: string, loc := #caller_location) {
		tokenizer := tokenize_from_string(str)
		errors := false
		for expected, i in tokens {
			value := tokenizer_next(&tokenizer)
			if fmt.aprint(value) != fmt.aprint(expected) {
				errors = true
				testing.errorf(
					t,
					"Expected %v-th token to be '%v' but got '%v'",
					i,
					expected,
					value,
					loc = loc,
				)
			} else {
				if errors {
					testing.errorf(t, "But %v-th was correct '%v'", i, value, loc = loc)
				}
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
			[]Token{
				TokRightArrow{},
				TokLeftArrow{},
				TokOperator{[]string{}, "<="},
				TokRightFatArrow{},
			},
			"-> <- <= =>",
		)
	}

	@(test)
	parse_arrow_like_operators :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokOperator{[]string{}, "-"},
				TokOperator{[]string{}, "<"},
				TokEquals{},
				TokPipe{},
			},
			" - < = | ",
		)
	}

	@(test)
	parse_colons :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokOperator{[]string{}, ":"},
				TokDoubleColon{},
				TokDoubleColon{},
				TokOperator{[]string{}, ":"},
			},
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

	@(test)
	parse_string_idents :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokUpperName{[]string{"A", "B"}, "C"},
				TokLowerName{[]string{"A", "B"}, "c"},
				TokOperator{[]string{"A", "B"}, "*"},
				TokSymbolName{[]string{"A", "B"}, "*"},
			},
			"A.B.C A.B.c A.B.* A.B.(*)",
		)
	}

	@(test)
	parse_string_idents_more_complex :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokUpperName{[]string{"AaAaaA", "BbBbbbB"}, "CcCcccc"},
				TokSymbolName{[]string{"Abra", "Cadabra"}, "<*>"},
			},
			"AaAaaA.BbBbbbB.CcCcccc Abra.Cadabra.(<*>)",
		)
	}

	@(test)
	parse_ints :: proc(t: ^testing.T) {
		expect_tokens(t, []Token{TokInt(123), TokInt(111), TokInt(17)}, "123 1_1__1___ 0x11")
	}

	@(test)
	parse_numbers :: proc(t: ^testing.T) {
		expect_tokens(
			t,
			[]Token{
				TokNumber(0.1),
				TokNumber(0.1),
				TokNumber(1.2),
				TokNumber(0.0001),
				TokNumber(2000),
			},
			"0.1 0._1 1.___2__ 1e-4 2e3",
		)
	}
}

