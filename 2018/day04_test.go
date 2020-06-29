package main

import (
	"io/ioutil"
	"testing"
)

const (
	SolutionD04Part1 = 101194
	SolutionD04Part2 = 102095
	exampleInput     = `[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
`
)

func TestD04Part1(t *testing.T) {
	expected := 240
	result := D04Part1(exampleInput)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, exampleInput)
	}
}

func TestD04Part2(t *testing.T) {
	expected := 4455
	result := D04Part2(exampleInput)
	if result != expected {
		t.Fatalf("%d != %d for input %s", result, expected, exampleInput)
	}
}

func TestD04AgainstPuzzleInput(t *testing.T) {
	input, err := ioutil.ReadFile("inputs/04")
	if err != nil {
		panic(err)
	}

	result1 := D04Part1(string(input))
	if result1 != SolutionD04Part1 {
		t.Errorf("%d != %d for part 1 puzzle input.", result1, SolutionD04Part1)
	}

	result2 := D04Part2(string(input))
	if result2 != SolutionD04Part2 {
		t.Errorf("%d != %d for part 2 puzzle input.", result2, SolutionD04Part2)
	}
}
