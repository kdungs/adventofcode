package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Firewall struct {
	config map[int]int
	size   int
}

func LoadFirewall(filename string) (*Firewall, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	firewall := Firewall{
		config: make(map[int]int),
		size:   0}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		groups := strings.Split(scanner.Text(), ": ")
		layer, err := strconv.Atoi(groups[0])
		if err != nil {
			return nil, err
		}
		depth, err := strconv.Atoi(groups[1])
		if err != nil {
			return nil, err
		}
		firewall.config[layer] = depth
		if layer > firewall.size {
			firewall.size = layer
		}
	}

	return &firewall, nil
}

func (fw Firewall) ScannerOnTop(layer int, step int) bool {
	depth, found := fw.config[layer]
	if !found {
		return false
	}

	upper := 2 * (depth - 1)

	pos := step % upper
	if pos >= depth {
		pos = upper - pos
	}

	return pos == 0
}

func (fw Firewall) TripSeverity(delay int) int {
	severity := 0
	for pos := 0; pos <= fw.size; pos++ {
		if fw.ScannerOnTop(pos, delay+pos) {
			severity += pos * fw.config[pos]
		}
	}
	return severity
}

func (fw Firewall) CalculatePassthroughDelay() int {
	delay := 0
	for fw.TripSeverity(delay) > 0 || fw.ScannerOnTop(0, delay) {
		delay++
	}
	return delay
}

func main() {
	fw, err := LoadFirewall("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d\n", fw.TripSeverity(0))
	fmt.Printf("%d\n", fw.TripSeverity(11596))
	fmt.Printf("%d\n", fw.CalculatePassthroughDelay())
}
