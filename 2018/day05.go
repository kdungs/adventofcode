package main

import (
	"strings"
	"unicode"
)

func react(a, b rune) bool {
	return a != b && unicode.ToLower(a) == unicode.ToLower(b)
}

func reduce(chain string) string {
	for i := 0; i < len(chain)-1; {
		if react(rune(chain[i]), rune(chain[i+1])) {
			chain = chain[:i] + chain[i+2:]
			i--
			if i < 0 {
				i = 0
			}
		} else {
			i++
		}
	}
	return chain
}

func remove(chain string, chr rune) string {
	lowerStr := string(chr)
	upperStr := string(unicode.ToUpper(chr))
	return strings.ReplaceAll(strings.ReplaceAll(chain, lowerStr, ""), upperStr, "")
}

// D05Part1 solves part 1 of day 5.
// https://adventofcode.com/2018/day/5
func D05Part1(input string) int {
	return len(reduce(input))
}

// D05Part2 solves part 2 of day 5.
// https://adventofcode.com/2018/day/5
func D05Part2(input string) int {
	reduced := reduce(input)
	minlen := len(reduced)
	for chr := 'a'; chr <= 'z'; chr++ {
		l := len(reduce(remove(reduced, chr)))
		if l < minlen {
			minlen = l
		}
	}
	return minlen
}
