package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func run() error {
	scanner := bufio.NewScanner(os.Stdin)
	vals := make([]int, 0)
	reg := 1
	for scanner.Scan() {
		line := scanner.Text()
		cmd := strings.Split(line, " ")
		if cmd[0] == "noop" {
			vals = append(vals, reg)
		} else if cmd[0] == "addx" {
			v, err := strconv.Atoi(cmd[1])
			if err != nil {
				return err
			}
			vals = append(vals, reg, reg)
			reg += v
		}
	}
	// Part 1
	total1 := 0
	for cycle := 20; cycle < len(vals); cycle += 40 {
		total1 += vals[cycle-1] * cycle
	}
	fmt.Printf("Part 1: %d\n", total1)
	// Part 2
	w := 40
	h := 6
	crt := make([]bool, w*h)
	for c, val := range vals {
		x := c % w
		pxl := (x-1 == val || x == val || x+1 == val)
		crt[c%(w*h)] = pxl
	}
	fmt.Print("Part 2:")
	for idx, pxl := range crt {
		if idx%w == 0 {
			fmt.Println()
		}
		if pxl {
			fmt.Print("█")
		} else {
			fmt.Print(" ")
		}
	}
	fmt.Println()
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
