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

func isBig(c *cave) bool {
	for _, r := range c.id {
		if !unicode.IsUpper(r) {
			return false
		}
	}
	return true
}

func (s *soln) countPaths(p1 bool) int {
	visited := make(map[*cave]bool)
	path := make([]string, 0)
	revisitUsed := false

	var visit func(c *cave) int
	visit = func(c *cave) int {
		path = append(path, c.id)
		defer func() {
			path = path[0 : len(path)-1]
		}()

		if visited[c] {
			revisitUsed = true
			defer func() {
				revisitUsed = false
			}()
		} else if !isBig(c) {
			visited[c] = true
			defer func() {
				delete(visited, c)
			}()
		}

		if c.id == "end" {
			return 1
		}
		paths := 0
		for _, n := range c.neighbors {
			if visited[n] && (p1 || revisitUsed) {
				continue
			}
			if n.id == "start" {
				continue
			}

			paths += visit(n)
		}
		return paths
	}
	return visit(s.caves["start"])
}

func (s *soln) Part1() int { return s.countPaths(true) }
func (s *soln) Part2() int { return s.countPaths(false) }

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
