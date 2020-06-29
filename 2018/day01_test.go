package main

import (
	"io/ioutil"
	"testing"
)

const (
	SolutionD01Part1 = 439
	SolutionD01Part2 = 124645
)

func testPart1(t *testing.T) {
	examples := []struct {
		input    string
		expected int
	}{
		{"+1\n-2\n+3\n+1\n", 3},
		{"+1\n+1\n+1\n", 3},
		{"+1\n+1\n-2\n", 0},
		{"-1\n-2\n-3\n", -6},
	}

	for _, example := range examples {
		result := D01Part1(example.input)
		if result != example.expected {
			t.Errorf("%d != %d for input %s", result, example.expected, example.input)
		}
	}
}

func testPart2(t *testing.T) {
	examples := []struct {
		input    string
		expected int
	}{
		{"+1\n-2\n+3\n+1\n", 2},
		{"+3\n+3\n+4\n-2\n-4\n", 10},
		{"-6\n+3\n+8\n+5\n-6\n", 5},
		{"+7\n+7\n-2\n-7\n-4\n", 14},
	}

	for _, example := range examples {
		result := D01Part2(example.input)
		if result != example.expected {
			t.Errorf("%d != %d for input %s", result, example.expected, example.input)
		}
	}
}

func TestD01AgainstPuzzleInput(t *testing.T) {
	input, err := ioutil.ReadFile("inputs/01")
	if err != nil {
		panic(err)
	}

	result1 := D01Part1(string(input))
	if result1 != SolutionD01Part1 {
		t.Errorf("%d != %d for part 1 puzzle input.", result1, SolutionD01Part1)
	}

	result2 := D01Part2(string(input))
	if result2 != SolutionD01Part2 {
		t.Errorf("%d != %d for part 2 puzzle input.", result2, SolutionD01Part2)
	}
}
