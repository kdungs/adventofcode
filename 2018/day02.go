package main

import (
	"strings"
)

func countLetters(input string) map[rune]int {
	counts := make(map[rune]int)
	for _, c := range input {
		counts[c]++
	}
	return counts
}

// D02Part1 solves part 1 of day 2
// https://adventofcode.com/2018/day/2
func D02Part1(input string) int {
	numTwo := 0
	numThree := 0
	for _, line := range strings.Split(input, "\n") {
		counts := countLetters(line)
		hasTwo := false
		hasThree := false
		for _, count := range counts {
			if count == 2 {
				hasTwo = true
			} else if count == 3 {
				hasThree = true
			}
		}
		if hasTwo {
			numTwo++
		}
		if hasThree {
			numThree++
		}
	}
	return numTwo * numThree
}

func hammingDistance(left, right string) int {
	if len(left) != len(right) {
		panic("We don't want to deal with strings of different lengths.")
	}
	if left == right {
		return 0
	}
	dist := 0
	for i := range left {
		if left[i] != right[i] {
			dist++
		}
	}
	return dist
}

func buildSolution(left, right string) string {
	s := ""
	for i := range left {
		if left[i] == right[i] {
			s += string(left[i])
		}
	}
	return s
}

// D02Part2 solves part 2 of day 2.
// https://adventofcode.com/2018/day/2
func D02Part2(input string) string {
	lines := strings.Split(input, "\n")
	for i, line := range lines[:len(lines)-1] {
		for _, other := range lines[i : len(lines)-1] {
			if hammingDistance(line, other) == 1 {
				return buildSolution(line, other)
			}
		}
	}
	return "NO SOLUTION"
}
