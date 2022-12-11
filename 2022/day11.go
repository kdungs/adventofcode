package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Op func(int) int

type Monkey struct {
	Items       []int
	Op          Op
	Mod         int
	TargetTrue  int
	TargetFalse int
	NumSeen     int
}

func (m *Monkey) Test(item int) bool {
	return item%m.Mod == 0
}

func (m *Monkey) Catch(item int) {
	m.Items = append(m.Items, item)
}

func parseStartingItems(line string) ([]int, error) {
	l := strings.TrimPrefix(line, "  Starting items: ")
	return toInts(strings.Split(l, ", "))
}

func parseOperation(line string) (Op, error) {
	l := strings.TrimPrefix(line, "  Operation: new = old ")
	if l == "* old" {
		return func(x int) int {
			return x * x
		}, nil
	}
	parts := strings.Split(l, " ")
	val, err := strconv.Atoi(parts[1])
	if err != nil {
		return nil, err
	}
	if parts[0] == "+" {
		return func(x int) int {
			return x + val
		}, nil
	}
	return func(x int) int {
		return x * val
	}, nil
}

func parseMod(line string) (int, error) {
	return strconv.Atoi(strings.TrimPrefix(line, "  Test: divisible by "))
}

func parseTarget(line string) (int, error) {
	l := strings.TrimPrefix(line, "    If true: throw to monkey ")
	l = strings.TrimPrefix(l, "    If false: throw to monkey ")
	return strconv.Atoi(l)
}

func parseMonkey(lines []string) (*Monkey, error) {
	items, err := parseStartingItems(lines[1])
	if err != nil {
		return nil, err
	}
	op, err := parseOperation(lines[2])
	if err != nil {
		return nil, err
	}
	mod, err := parseMod(lines[3])
	if err != nil {
		return nil, err
	}
	targetTrue, err := parseTarget(lines[4])
	if err != nil {
		return nil, err
	}
	targetFalse, err := parseTarget(lines[5])
	if err != nil {
		return nil, err
	}
	return &Monkey{
		Items:       items,
		Op:          op,
		Mod:         mod,
		TargetTrue:  targetTrue,
		TargetFalse: targetFalse,
	}, nil
}

func parseMonkeys(lines []string) ([]Monkey, error) {
	monkeys := make([]Monkey, 0, len(lines)/7)
	for i := 0; i < len(lines); i += 7 {
		m, err := parseMonkey(lines[i : i+7])
		if err != nil {
			return nil, err
		}
		monkeys = append(monkeys, *m)
	}
	return monkeys, nil
}

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

func playRound(ms []Monkey, div int, mod int) {
	for midx, m := range ms {
		for _, i := range m.Items {
			ms[midx].NumSeen++
			i := (m.Op(i) / div) % mod
			if m.Test(i) {
				ms[m.TargetTrue].Catch(i)
			} else {
				ms[m.TargetFalse].Catch(i)
			}
		}
		ms[midx].Items = make([]int, 0)
	}
}

func play(lines []string, rounds, div int) (int, error) {
	monkeys, err := parseMonkeys(lines)
	if err != nil {
		return -1, err
	}
	mod := 1
	for _, m := range monkeys {
		mod *= m.Mod
	}
	for r := 0; r < rounds; r++ {
		playRound(monkeys, div, mod)
	}
	act := make([]int, 0, len(monkeys))
	for _, m := range monkeys {
		act = append(act, m.NumSeen)
	}
	sort.Ints(act)
	return act[len(act)-1] * act[len(act)-2], nil
}

func run() error {
	lines, err := readLines(os.Stdin)
	if err != nil {
		return err
	}
	score1, err := play(lines, 20, 3)
	if err != nil {
		return err
	}
	fmt.Printf("Part 1: %d\n", score1)
	score2, err := play(lines, 10000, 1)
	if err != nil {
		return err
	}
	fmt.Printf("Part 2: %d\n", score2)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
