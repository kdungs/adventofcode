package main

import (
	"bufio"
	"fmt"
	"os"
)

func common(lhs, rhs string) (rune, bool) {
	// Quadratic; optimize later if needed.
	for _, l := range lhs {
		for _, r := range rhs {
			if l == r {
				return l, true
			}
		}
	}
	return ' ', false
}

func common3(a, b, c string) (rune, bool) {
	// ¯\_(ツ)_/¯
	// We're scanning short strings. This is probably not too bad.
	for _, ca := range a {
		for _, cb := range b {
			if ca != cb {
				continue
			}
			for _, cc := range c {
				if cc == ca {
					return cc, true
				}
			}
		}
	}
	return ' ', false
}

func prio(c rune) int {
	if c >= 'a' && c <= 'z' {
		return int(c-'a') + 1
	}
	return int(c-'A') + 27
}

func part1(rucksacks []string) int {
	total := 0
	for _, line := range rucksacks {
		n := len(line)
		lhs, rhs := line[:n/2], line[n/2:]
		c, ok := common(lhs, rhs)
		if !ok {
			continue
		}
		total += prio(c)
	}
	return total
}

func part2(rucksacks []string) int {
	total := 0
	for i := 0; i < len(rucksacks); i += 3 {
		c, ok := common3(rucksacks[i], rucksacks[i+1], rucksacks[i+2])
		if !ok {
			continue
		}
		total += prio(c)
	}
	return total
}

func main() {
	rucksacks := make([]string, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		rucksacks = append(rucksacks, scanner.Text())
	}
	fmt.Printf("Part1: %d\n", part1(rucksacks))
	fmt.Printf("Part2: %d\n", part2(rucksacks))
}
