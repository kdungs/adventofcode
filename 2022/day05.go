package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"regexp"
	"strconv"
)

func readLines(r io.Reader) ([]string, error) {
	lines := make([]string, 0)
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return nil, err
		}
		lines = append(lines, scanner.Text())
	}
	return lines, nil
}

func toInts(strs []string) ([]int, error) {
	ints := make([]int, len(strs))
	for idx, s := range strs {
		i, err := strconv.Atoi(s)
		if err != nil {
			return nil, err
		}
		ints[idx] = i
	}
	return ints, nil
}

func pos(lines []string, sep string) int {
	for i, line := range lines {
		if line == sep {
			return i
		}
	}
	return -1
}

func parseStacks(lines []string) [][]byte {
	// Determine shape
	h := len(lines)
	w := (len(lines[0]) + 1) / 4
	results := make([][]byte, 0, w)
	for x := 0; x < w; x++ {
		px := 1 + 4*x
		stack := make([]byte, 0, h-1)
		for y := h - 2; y >= 0; y-- {
			c := lines[y][px]
			if c != ' ' {
				stack = append(stack, c)
			}
		}
		results = append(results, stack)
	}
	return results
}

func move1(stacks [][]byte, num int, from int, to int) {
	for i := 0; i < num; i++ {
		p := len(stacks[from]) - 1
		x := stacks[from][p]
		stacks[from] = stacks[from][:p]
		stacks[to] = append(stacks[to], x)
	}
}

func move2(stacks [][]byte, num int, from int, to int) {
	s := len(stacks[from]) - num
	take := stacks[from][s:]
	stacks[from] = stacks[from][:s]
	stacks[to] = append(stacks[to], take...)
}

func tops(stacks [][]byte) string {
	tops := make([]byte, len(stacks))
	for i, stack := range stacks {
		tops[i] = stack[len(stack)-1]
	}
	return string(tops)
}

func run() error {
	lines, err := readLines(os.Stdin)
	if err != nil {
		return err
	}
	sep := pos(lines, "")
	stacks1 := parseStacks(lines[:sep])
	stacks2 := parseStacks(lines[:sep])
	re := regexp.MustCompile(`^move (\d+) from (\d+) to (\d+)$`)
	for _, command := range lines[sep+1:] {
		m := re.FindStringSubmatch(command)
		pos, err := toInts(m[1:])
		if err != nil {
			return err
		}
		move1(stacks1, pos[0], pos[1]-1, pos[2]-1)
		move2(stacks2, pos[0], pos[1]-1, pos[2]-1)
	}

	fmt.Printf("Part1: %s\n", tops(stacks1))
	fmt.Printf("Part2: %s\n", tops(stacks2))
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
