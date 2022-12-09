package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type V struct {
	X int
	Y int
}

type N struct {
	V    V
	Prev *N
	Next *N
}

// Don't call on Head.
func (n *N) Follow() {
	dx := n.V.X - n.Prev.V.X
	dx2 := dx * dx
	dy := n.V.Y - n.Prev.V.Y
	dy2 := dy * dy

	if dx2 <= 1 && dy2 <= 1 {
		// Touching!
		if n.Next != nil {
			n.Next.Follow()
		}
		return
	}

	if dx > 0 {
		n.V.X--
	} else if dx < 0 {
		n.V.X++
	}
	if dy > 0 {
		n.V.Y--
	} else if dy < 0 {
		n.V.Y++
	}

	if n.Next != nil {
		n.Next.Follow()
	}
}

type L struct {
	Head *N
	Tail *N
}

func (l *L) Move(dir string) {
	switch dir {
	case "U":
		l.Head.V.Y++
	case "R":
		l.Head.V.X++
	case "D":
		l.Head.V.Y--
	case "L":
		l.Head.V.X--
	default:
		panic("not a dir")
	}
	l.Head.Next.Follow()
}

func MakeL(size int) *L {
	pos := V{0, 0}
	head := &N{V: pos}
	prev := head
	for i := 1; i < size; i++ {
		cur := &N{V: pos, Prev: prev}
		prev.Next = cur
		prev = cur
	}
	return &L{
		Head: head,
		Tail: prev,
	}
}
func main() {
	l1 := MakeL(2)
	seen1 := map[V]bool{
		l1.Tail.V: true,
	}
	l2 := MakeL(10)
	seen2 := map[V]bool{
		l2.Tail.V: true,
	}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		cmd := strings.Split(scanner.Text(), " ")
		dir := cmd[0]
		num, err := strconv.Atoi(cmd[1])
		if err != nil {
			log.Fatalf("%v", err)
		}
		for i := 0; i < num; i++ {
			l1.Move(dir)
			seen1[l1.Tail.V] = true
			l2.Move(dir)
			seen2[l2.Tail.V] = true
		}
	}
	fmt.Printf("Part 1: %d\n", len(seen1))
	fmt.Printf("Part 2: %d\n", len(seen2))
}
