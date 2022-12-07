package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type File struct {
	Name   string
	Parent *Dir
	Size   int
}

type Dir struct {
	Name   string
	Parent *Dir
	Dirs   map[string]*Dir
	Files  map[string]*File
	size   *int
}

func NewDir(name string, parent *Dir) *Dir {
	return &Dir{
		Name:   name,
		Parent: parent,
		Dirs:   make(map[string]*Dir),
		Files:  make(map[string]*File),
	}
}

func (d *Dir) AddFile(name string, size int) *File {
	f := &File{
		Name:   name,
		Parent: d,
		Size:   size,
	}
	d.Files[name] = f
	return f
}

func (d *Dir) AddDir(name string) *Dir {
	dir := NewDir(name, d)
	d.Dirs[name] = dir
	return dir
}

func (d *Dir) Size() int {
	if d.size != nil {
		return *d.size
	}
	size := 0
	for _, file := range d.Files {
		size += file.Size
	}
	for _, dir := range d.Dirs {
		size += dir.Size()
	}
	d.size = &size
	return size
}

func (d *Dir) Walk(fn func(*Dir)) {
	queue := []*Dir{d}
	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		fn(cur)
		for _, dir := range cur.Dirs {
			queue = append(queue, dir)
		}
	}
}

func run() error {
	scanner := bufio.NewScanner(os.Stdin)
	root := NewDir("", nil)
	pwd := root
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")
		if parts[0] == "$" {
			// Commands
			if parts[1] == "ls" {
				continue
			}
			// Change directory
			if parts[2] == ".." {
				pwd = pwd.Parent
			} else if parts[2] == "/" {
				pwd = root
			} else {
				pwd = pwd.Dirs[parts[2]]
			}
		} else {
			// Listing
			name := parts[1]
			if parts[0] == "dir" {
				// Directory
				pwd.AddDir(name)
			} else {
				// File
				size, err := strconv.Atoi(parts[0])
				if err != nil {
					return err
				}
				pwd.AddFile(name, size)
			}
		}
	}

	// Part 1
	total1 := 0
	root.Walk(func(d *Dir) {
		size := d.Size()
		if size <= 100000 {
			total1 += size
		}
	})
	fmt.Printf("Part 1: %d\n", total1)

	// Part 2
	all := 70000000
	need := 30000000
	used := root.Size()
	free := all - used
	min := used
	root.Walk(func(d *Dir) {
		size := d.Size()
		freeWithout := free + size
		if freeWithout >= need && size < min {
			min = size
		}
	})
	fmt.Printf("Part 2: %d\n", min)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
