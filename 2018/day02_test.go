package main

import (
	"io/ioutil"
	"testing"
)

const (
	SolutionD02Part1 = 9633
	SolutionD02Part2 = "lujnogabetpmsydyfcovzixaw"
)

func TestD02Part1(t *testing.T) {
	input := `abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab
`
	expected := 12
	result := D02Part1(input)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, input)
	}
}

func TestD02Part2(t *testing.T) {
	input := `abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
`
	expected := "fgij"
	result := D02Part2(input)
	if result != expected {
		t.Fatalf("%s != %s for input %s", result, expected, input)
	}
}

func TestD02AgainstPuzzleInput(t *testing.T) {
	input, err := ioutil.ReadFile("inputs/02")
	if err != nil {
		panic(err)
	}

	result1 := D02Part1(string(input))
	if result1 != SolutionD02Part1 {
		t.Errorf("%d != %d for part 1 puzzle input.", result1, SolutionD02Part1)
	}

	result2 := D02Part2(string(input))
	if result2 != SolutionD02Part2 {
		t.Errorf("%s != %s for part 2 puzzle input.", result2, SolutionD02Part2)
	}
}
