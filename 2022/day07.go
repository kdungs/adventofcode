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
	Parent *Dir
	Size   int
}

type Dir struct {
	Parent *Dir
	Dirs   map[string]*Dir
	Files  map[string]*File
	size   *int
}

func NewDir(parent *Dir) *Dir {
	return &Dir{
		Parent: parent,
		Dirs:   make(map[string]*Dir),
		Files:  make(map[string]*File),
	}
}

func (d *Dir) AddFile(name string, size int) *File {
	f := &File{
		Parent: d,
		Size:   size,
	}
	d.Files[name] = f
	return f
}

func (d *Dir) AddDir(name string) *Dir {
	dir := NewDir(d)
	d.Dirs[name] = dir
	return dir
}

func run() error {
	scanner := bufio.NewScanner(os.Stdin)
	root := NewDir(nil)
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
	fmt.Println(root)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("run(): %v", err)
	}
}
