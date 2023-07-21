package purrloin

import "core:testing"
import "core:fmt"

main :: proc () {
		tokenizer := tokenize_from_string("1.__2 0__._1")
		for i in 0..=10 {
			value := tokenizer_next_spanned(&tokenizer)
      fmt.println("", i, value)
		}
}
