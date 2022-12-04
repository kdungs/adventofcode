package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

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

func run() error {
	re := regexp.MustCompile(`^(\d+)-(\d+),(\d+)-(\d+)$`)
	scanner := bufio.NewScanner(os.Stdin)
	total1 := 0
	total2 := 0
	for scanner.Scan() {
		line := scanner.Text()
		m := re.FindStringSubmatch(line)
		rs, err := toInts(m[1:])
		if err != nil {
			return err
		}
		if (rs[0] <= rs[2] && rs[1] >= rs[3]) || (rs[2] <= rs[0] && rs[3] >= rs[1]) {
			total1++
		}
		if (rs[0] <= rs[3] && rs[1] >= rs[2]) || (rs[3] <= rs[0] && rs[2] >= rs[1]) {
			total2++
		}
	}
	fmt.Printf("Part1: %d\n", total1)
	fmt.Printf("Part2: %d\n", total2)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
