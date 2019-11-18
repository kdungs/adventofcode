package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

type Graph map[string][]string

func (g Graph) BfsCount(node string) int {
	visited := make(map[string]bool)
	queue := []string{node}
	count := 0

	for len(queue) > 0 {
		next := queue[0]
		queue = queue[1:]

		if !visited[next] {
			visited[next] = true
			count += 1
			queue = append(queue, g[next]...)
		}
	}

	return count
}

type UnionFind struct {
	parent map[string]string
	rank   map[string]int
	groups int
}

func NewUnionFind() *UnionFind {
	return &UnionFind{
		parent: make(map[string]string),
		rank:   make(map[string]int),
		groups: 0}
}

func (uf *UnionFind) Find(s string) string {
	if parent, ok := uf.parent[s]; ok {
		if parent == s {
			return s
		}
		return uf.Find(parent)
	}

	// Key not found in map. Insert new element, basically MakeSet(s).
	uf.parent[s] = s
	uf.rank[s] = 0
	uf.groups += 1
	return s
}

func (uf *UnionFind) Union(a string, b string) {
	aroot := uf.Find(a)
	broot := uf.Find(b)

	if aroot == broot {
		return
	}

	arank := uf.rank[aroot]
	brank := uf.rank[broot]

	if arank < brank {
		uf.parent[aroot] = broot
	} else if arank > brank {
		uf.parent[broot] = aroot
	} else {
		uf.parent[broot] = aroot
		uf.rank[aroot] += 1
	}
	uf.groups -= 1
}

func (uf UnionFind) NumGroups() int {
	return uf.groups
}

func (g Graph) ConnectedGroups() *UnionFind {
	uf := NewUnionFind()
	for node, neighbours := range g {
		for i := range neighbours {
			neighbour := neighbours[i]
			uf.Union(node, neighbour)
		}
	}
	return uf
}

func main() {
	file, err := os.Open("input.txt")
	Check(err)

	g := make(Graph)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		groups := strings.Split(scanner.Text(), " <-> ")
		node := groups[0]
		neighbours := strings.Split(groups[1], ", ")
		g[node] = neighbours
	}
	Check(scanner.Err())
	fmt.Printf("%d\n", g.BfsCount("0"))

	uf := g.ConnectedGroups()
	fmt.Printf("%d\n", uf.NumGroups())
}
