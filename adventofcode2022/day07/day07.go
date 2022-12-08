package day07

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type lsEntry interface {
	IsLsEntry()
}

type dirLsEntry string
type fileLsEntry struct {
	size int
	name string
}

func (e *dirLsEntry) IsLsEntry()  {}
func (e *fileLsEntry) IsLsEntry() {}

type cmd interface {
	IsCmd()
}

type cdCmd string
type lsCmd []lsEntry

func (c *cdCmd) IsCmd() {}
func (c *lsCmd) IsCmd() {}

func must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func parseLsEntry(s string) lsEntry {
	switch {
	case strings.HasPrefix(s, "dir "):
		d := dirLsEntry(strings.TrimPrefix(s, "dir "))
		return &d
	default:
		f := &fileLsEntry{}
		_, err := fmt.Sscanf(s, "%d %s", &f.size, &f.name)
		must(err)
		return f
	}
}

func parseCmds(s *bufio.Scanner) (r []cmd) {
	ls := &lsCmd{}
	for {
		done := !s.Scan()
		if (done || strings.HasPrefix(s.Text(), "$")) && len(*ls) > 0 {
			r = append(r, ls)
			ls = &lsCmd{}
		}
		if done {
			break
		}

		switch {
		case !strings.HasPrefix(s.Text(), "$"):
			*ls = append(*ls, parseLsEntry(s.Text()))
		case strings.HasPrefix(s.Text(), "$ cd "):
			cd := cdCmd(strings.TrimPrefix(s.Text(), "$ cd "))
			r = append(r, &cd)
		case s.Text() == "$ ls":
		default:
			must(fmt.Errorf("invalid cmd line: %s", s.Text()))
		}
	}
	return
}

type fsNode struct {
	name     string
	parent   *fsNode
	files    map[string]*fileLsEntry
	children map[string]*fsNode
}

func newFsNode(name string, parent *fsNode) *fsNode {
	return &fsNode{
		name:     name,
		parent:   parent,
		files:    map[string]*fileLsEntry{},
		children: map[string]*fsNode{}}
}

func buildFs(cmds []cmd) *fsNode {
	r := newFsNode("/", nil)
	if string(*cmds[0].(*cdCmd)) != r.name {
		must(fmt.Errorf("expected first command to cd %s, got: %v", r.name, cmds[0]))
	}
	for _, cmd := range cmds {
		switch c := cmd.(type) {
		case *cdCmd:
			name := string(*c)
			switch name {
			case "..":
				r = r.parent
			default:
				if r.children[name] == nil {
					r.children[name] = newFsNode(name, r)
				}
				r = r.children[name]
			}
		case *lsCmd:
			for _, e := range *c {
				f, ok := e.(*fileLsEntry)
				if !ok {
					continue
				}
				if r.files[f.name] == nil {
					r.files[f.name] = f
				} else if r.files[f.name].size != f.size {
					must(fmt.Errorf("file %s in dir %s changed size: %d -> %d", f.name, r.name, r.files[f.name].size, f.size))
				}
			}
		}
	}
	for r.parent != nil {
		r = r.parent
	}
	return r
}

func part1(n *fsNode) (size, agg int) {
	for _, n := range n.children {
		s, a := part1(n)
		size += s
		agg += a
	}
	for _, f := range n.files {
		size += f.size
	}
	if size <= 100_000 {
		agg += size
	}
	return
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	fs := buildFs(parseCmds(s))
	if isPart1 {
		_, agg := part1(fs)
		return agg, nil
	}
	return 0, nil
}
