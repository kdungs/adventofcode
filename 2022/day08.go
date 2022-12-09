package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

type Forest struct {
	Trees  []string
	Height int
	Width  int
}

func NewForest(trees []string) *Forest {
	return &Forest{
		Trees:  trees,
		Height: len(trees),
		Width:  len(trees[0]),
	}
}

func (f *Forest) At(x, y int) byte {
	return f.Trees[y][x]
}

// ViewWest returns the number of trees towards the west that can be seen from
// (x, y) and whether the view in that direction is unobstructed.
func (f *Forest) ViewWest(x, y int) (int, bool) {
	t := f.At(x, y)
	for i := 1; x-i >= 0; i++ {
		if t <= f.At(x-i, y) {
			return i, false
		}
	}
	return x, true
}

// ViewNorth returns the number of trees towards the north that can be seen
// from (x, y) and whether the view in that direction is unobstructed.
func (f *Forest) ViewNorth(x, y int) (int, bool) {
	t := f.At(x, y)
	for i := 1; y-i >= 0; i++ {
		if t <= f.At(x, y-i) {
			return i, false
		}
	}
	return y, true
}

// ViewEast returns the number of trees towards the east that can be seen from
// (x, y) and whether the view in that direction is unobstructed.
func (f *Forest) ViewEast(x, y int) (int, bool) {
	t := f.At(x, y)
	for i := 1; x+i < f.Width; i++ {
		if t <= f.At(x+i, y) {
			return i, false
		}
	}
	return f.Width - 1 - x, true
}

// ViewSouth returns the number of trees towards the south that can be seen
// from (x, y) and whether the view in that direction is unobstructed.
func (f *Forest) ViewSouth(x, y int) (int, bool) {
	t := f.At(x, y)
	for i := 1; y+i < f.Height; i++ {
		if t <= f.At(x, y+i) {
			return i, false
		}
	}
	return f.Height - 1 - y, true
}

func readLines(r io.Reader) ([]string, error) {
	scanner := bufio.NewScanner(r)
	lines := make([]string, 0)
	for scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return nil, err
		}
		lines = append(lines, scanner.Text())
	}
	return lines, nil
}

func main() {
	trees, err := readLines(os.Stdin)
	if err != nil {
		log.Fatalf("%v", err)
	}
	f := NewForest(trees)

	total1 := 0
	max2 := 0
	for y := 0; y < f.Height; y++ {
		for x := 0; x < f.Width; x++ {
			numW, visW := f.ViewWest(x, y)
			numN, visN := f.ViewNorth(x, y)
			numE, visE := f.ViewEast(x, y)
			numS, visS := f.ViewSouth(x, y)

			// Part 1
			if visW || visN || visE || visS {
				total1++
			}

			// Part 2
			s := numW * numN * numE * numS
			if s > max2 {
				max2 = s
			}
		}
	}
	fmt.Printf("Part 1: %d\n", total1)
	fmt.Printf("Part 2: %d\n", max2)
}
