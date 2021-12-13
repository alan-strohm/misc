package day12

import (
	"bufio"
	"strings"
	"unicode"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type cave struct {
	id        string
	neighbors []*cave
}

type soln struct {
	caves map[string]*cave
}

type pred func(*cave) bool

var isBig = pred(func(c *cave) bool {
	for _, r := range c.id {
		if !unicode.IsUpper(r) {
			return false
		}
	}
	return true
})

func (s *soln) countPaths(canRevisit pred) int {
	visited := make(map[*cave]bool)
	path := make([]*cave, 0)

	var visit func(c *cave) int
	visit = func(c *cave) int {
		path = append(path, c)
		if !canRevisit(c) {
			visited[c] = true
		}
		defer func() {
			path = path[0 : len(path)-1]
			delete(visited, c)
		}()

		if c.id == "end" {
			return 1
		}
		paths := 0
		for _, n := range c.neighbors {
			if visited[n] {
				continue
			}
			paths += visit(n)
		}
		return paths
	}
	return visit(s.caves["start"])
}

func (s *soln) Part1() int { return s.countPaths(isBig) }
func (s *soln) Part2() int { return 0 }

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &soln{caves: make(map[string]*cave)}
	for scanner.Scan() {
		ids := strings.Split(scanner.Text(), "-")
		cs := make([]*cave, len(ids))
		for i, id := range ids {
			c, ok := r.caves[id]
			if !ok {
				c = &cave{id: id}
				r.caves[id] = c
			}
			cs[i] = c
		}
		for i, c := range cs {
			if i+1 != len(cs) {
				c.neighbors = append(c.neighbors, cs[i+1])
			}
			if i != 0 {
				c.neighbors = append(c.neighbors, cs[i-1])
			}
		}
	}
	return r, nil
}
