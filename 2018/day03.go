package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var regex = regexp.MustCompile(`^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$`)

type claim struct {
	ID int
	X  int
	Y  int
	W  int
	H  int
}

func claimFromString(str string) (*claim, error) {
	ms := regex.FindAllStringSubmatch(str, -1)
	if len(ms) != 1 {
		return nil, fmt.Errorf("unable to create claim from string '%s'", str)
	}
	m := ms[0]
	if len(m) != 6 {
		return nil, fmt.Errorf("couldn't find all required field in match %+v", m)
	}
	id, err := strconv.Atoi(m[1])
	if err != nil {
		return nil, err
	}
	x, err := strconv.Atoi(m[2])
	if err != nil {
		return nil, err
	}
	y, err := strconv.Atoi(m[3])
	if err != nil {
		return nil, err
	}
	w, err := strconv.Atoi(m[4])
	if err != nil {
		return nil, err
	}
	h, err := strconv.Atoi(m[5])
	if err != nil {
		return nil, err
	}
	return &claim{
		ID: id,
		X:  x,
		Y:  y,
		W:  w,
		H:  h,
	}, nil
}

func claimsFromString(input string) ([]claim, error) {
	lines := strings.Split(strings.TrimSuffix(input, "\n"), "\n")
	claims := make([]claim, 0, len(lines))
	for _, line := range lines {
		cl, err := claimFromString(line)
		if err != nil {
			return []claim{}, err
		}
		claims = append(claims, *cl)
	}
	return claims, nil
}

type point struct {
	X int
	Y int
}

func countOverlaps(claims []claim) map[point]int {
	overlaps := make(map[point]int)
	for _, cl := range claims {
		for dy := 0; dy < cl.H; dy++ {
			y := cl.Y + dy
			for dx := 0; dx < cl.W; dx++ {
				x := cl.X + dx
				overlaps[point{X: x, Y: y}]++
			}
		}
	}
	return overlaps
}

// D03Part1 solves part 1 of day 3.
// https://adventofcode.com/2018/day/3
func D03Part1(input string) int {
	claims, err := claimsFromString(input)
	if err != nil {
		panic(err)
	}
	overlaps := countOverlaps(claims)
	inMoreThanOne := 0
	for _, num := range overlaps {
		if num > 1 {
			inMoreThanOne++
		}
	}
	return inMoreThanOne
}

func isSingular(cl claim, overlaps map[point]int) bool {
	for dy := 0; dy < cl.H; dy++ {
		y := cl.Y + dy
		for dx := 0; dx < cl.W; dx++ {
			x := cl.X + dx
			p := point{X: x, Y: y}
			if overlaps[p] != 1 {
				return false
			}
		}
	}
	return true
}

// D03Part2 solves part 2 of day 3.
// https://adventofcode.com/2018/day/3
func D03Part2(input string) int {
	claims, err := claimsFromString(input)
	if err != nil {
		panic(err)
	}
	overlaps := countOverlaps(claims)
	for _, cl := range claims {
		if isSingular(cl, overlaps) {
			return cl.ID
		}
	}
	return -1
}
