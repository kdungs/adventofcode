package main

import (
	"io/ioutil"
	"strings"
	"testing"
)

const (
	SolutionD05Part1 = 9296
	SolutionD05Part2 = 5534
)

func TestD05Part1(t *testing.T) {
	if D05Part1("aA") != 0 {
		t.Fatalf("aA")
	}
	if D05Part1("abBA") != 0 {
		t.Fatalf("abBA")
	}
	if D05Part1("abAB") != 4 {
		t.Fatalf("abAB")
	}
	if D05Part1("aabAAB") != 6 {
		t.Fatalf("aabAAB")
	}

	exampleInput := "dabAcCaCBAcCcaDA"
	expected := 10
	result := D05Part1(exampleInput)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, exampleInput)
	}
}

func TestD05Part2(t *testing.T) {
	exampleInput := "dabAcCaCBAcCcaDA"
	expected := 4
	result := D05Part2(exampleInput)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, exampleInput)
	}
}

func TestD05AgainstPuzzleInput(t *testing.T) {
	inputbytes, err := ioutil.ReadFile("inputs/05")
	input := strings.TrimSpace(string(inputbytes))
	if err != nil {
		panic(err)
	}

	result1 := D05Part1(input)
	if result1 != SolutionD05Part1 {
		t.Errorf("%d != %d for part 1 puzzle input.", result1, SolutionD05Part1)
	}

	result2 := D05Part2(string(input))
	if result2 != SolutionD05Part2 {
		t.Errorf("%d != %d for part 2 puzzle input.", result2, SolutionD05Part2)
	}
}
