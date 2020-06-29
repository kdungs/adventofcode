package main

import (
	"io/ioutil"
	"testing"
)

const (
	SolutionD03Part1 = 112418
	SolutionD03Part2 = 560
)

func TestD03Part1(t *testing.T) {
	input := `#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
`
	expected := 4
	result := D03Part1(input)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, input)
	}

}
func TestD03Part2(t *testing.T) {}

func TestD03AgainstPuzzleInput(t *testing.T) {
	input, err := ioutil.ReadFile("inputs/03")
	if err != nil {
		panic(err)
	}

	result1 := D03Part1(string(input))
	if result1 != SolutionD03Part1 {
		t.Errorf("%d != %d for part 1 puzzle input.", result1, SolutionD03Part1)
	}

	result2 := D03Part2(string(input))
	if result2 != SolutionD03Part2 {
		t.Errorf("%d != %d for part 2 puzzle input.", result2, SolutionD03Part2)
	}
}
