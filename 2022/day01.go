package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func run() error {
	scanner := bufio.NewScanner(os.Stdin)
	top3 := []int{0, 0, 0}
	cur := 0
	for scanner.Scan() {
		txt := scanner.Text()
		if txt == "" {
			if cur > top3[0] {
				top3[0] = cur
				sort.Ints(top3)
			}
			cur = 0
		} else {
			i, err := strconv.Atoi(txt)
			if err != nil {
				return err
			}
			cur += i
		}
	}
	if cur > top3[0] {
		top3[0] = cur
		sort.Ints(top3)
	}
	fmt.Printf("Part1: %d\n", top3[2])
	fmt.Printf("Part2: %d\n", top3[0]+top3[1]+top3[2])
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
