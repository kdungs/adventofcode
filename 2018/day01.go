package main

import (
	"strconv"
	"strings"
)

// D01Part1 solves part 1 of day 1.
// https://adventofcode.com/2018/day/1
func D01Part1(input string) int {
	strs := strings.Split(input, "\n")
	res := 0
	for _, str := range strs[:len(strs)-1] {
		val, err := strconv.Atoi(str)
		if err != nil {
			panic(err)
		}
		res += val
	}
	return res
}

// D01Part2 solves part 2 of day 1.
// https://adventofcode.com/2018/day/1
func D01Part2(input string) int {
	strs := strings.Split(input, "\n")
	seen := make(map[int]bool)
	seen[0] = true
	res := 0
	for {
		for _, str := range strs[:len(strs)-1] {
			val, err := strconv.Atoi(str)
			if err != nil {
				panic(err)
			}
			res += val
			_, found := seen[res]
			if found {
				return res
			}
			seen[res] = true
		}
	}
}
