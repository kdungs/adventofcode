package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Register struct {
	Regs map[string]int
}

func NewRegister() Register {
	rs := Register{}
	rs.Regs = make(map[string]int)
	return rs
}

func (rs *Register) Increment(addr string, amount int) {
	rs.Regs[addr] += amount
}

func (rs *Register) Value(addr string) int {
	return rs.Regs[addr]
}

func (rs *Register) Max() int {
	max := 0
	for _, v := range rs.Regs {
		if v > max {
			max = v
		}
	}
	return max
}

func EvalComparison(cmp string, a int, b int) (bool, error) {
	switch cmp {
	case "==":
		return a == b, nil
	case "!=":
		return a != b, nil
	case "<":
		return a < b, nil
	case "<=":
		return a <= b, nil
	case ">":
		return a > b, nil
	case ">=":
		return a >= b, nil
	default:
		return false, errors.New("Not a good comparison.")
	}
}

func main() {
	file, err := os.Open("input/08.txt")
	if err != nil {
		log.Fatal(err)
	}

	rs := NewRegister()
	scanner := bufio.NewScanner(file)
	allmax := 0
	for scanner.Scan() {
		line := scanner.Text()
		tokens := strings.Split(line, " ")
		addr := tokens[0]
		value, err := strconv.Atoi(tokens[2])
		if err != nil {
			log.Fatal("Not a number %s", tokens[2])
		}
		if tokens[1] == "dec" {
			value *= -1
		}
		cmpAddr := tokens[4]
		cmpOp := tokens[5]
		cmpValue, err := strconv.Atoi(tokens[6])
		if err != nil {
			log.Fatal("Not a number %s", tokens[6])
		}

		cmp, err := EvalComparison(cmpOp, rs.Value(cmpAddr), cmpValue)
		if err != nil {
			log.Fatal(err)
		}
		if cmp {
			rs.Increment(addr, value)
			val := rs.Value(addr)
			if val > allmax {
				allmax = val
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d\n", rs.Max())
	fmt.Printf("%d\n", allmax)
}
