package main

import (
	"fmt"
	"io"
	"log"
	"os"
)

func allDifferent(s string) bool {
	chrs := make(map[rune]bool)
	for _, c := range s {
		if _, ok := chrs[c]; ok {
			return false
		}
		chrs[c] = true
	}
	return true
}

func markerPosition(s string, size int) int {
	for i := size; i < len(s); i++ {
		if allDifferent(s[i-size : i]) {
			return i
		}
	}
	return -1
}

func main() {
	cases := []struct {
		Input     string
		Expected  int
		Expected2 int
	}{
		{"mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19},
		{"bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23},
		{"nppdvjthqldpwncqszvftbrmjlhg", 6, 23},
		{"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29},
		{"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26},
	}

	for _, tc := range cases {
		got := markerPosition(tc.Input, 4)
		if got != tc.Expected {
			log.Fatalf("markerPosition(%q, 4): want %d, got %d", tc.Input, tc.Expected, got)
		}
		got2 := markerPosition(tc.Input, 14)
		if got2 != tc.Expected2 {
			log.Fatalf("markerPosition(%q, 14): want %d, got %d", tc.Input, tc.Expected2, got2)
		}
	}

	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatalf("%v", err)
	}
	fmt.Printf("Part 1: %d\n", markerPosition(string(data), 4))
	fmt.Printf("Part 2: %d\n", markerPosition(string(data), 14))
}
