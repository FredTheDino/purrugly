package purrloin

import "core:testing"
import "core:fmt"

main :: proc () {
		tokenizer := tokenize_from_string("( < <= (")
		for i in 0..=10 {
			value := tokenizer_next(&tokenizer)
      fmt.println("", i, value)
		}
}
