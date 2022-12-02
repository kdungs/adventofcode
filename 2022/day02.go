package main

import (
	"bufio"
	"fmt"
	"os"
)

func playerScore(player int) int {
	return player + 1
}

func resultScore(result int) int {
	return result * 3
}

func score1(opponent int, player int) int {
	result := ((player - opponent) + 1 + 3) % 3
	return playerScore(player) + resultScore(result)
}

func score2(opponent int, result int) int {
	player := (opponent + result + 2) % 3
	return playerScore(player) + resultScore(result)
}

func main() {
	total1 := 0
	total2 := 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		lhs := int(line[0] - 'A')
		rhs := int(line[2] - 'X')
		total1 += score1(lhs, rhs)
		total2 += score2(lhs, rhs)
	}
	fmt.Printf("Part1: %d\n", total1)
	fmt.Printf("Part2: %d\n", total2)
}
