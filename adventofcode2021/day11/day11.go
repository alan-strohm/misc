package day11

import (
	"bufio"
	"fmt"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type soln struct {
	byLevel [10]map[point]*state
	all     map[point]*state
	steps   int
	flashes int
}

const dim = 10

type point struct{ x, y int }

func (p point) neighbors() []point {
	oob := func(n int) bool { return n < 0 || n >= dim }
	r := []point{}
	for _, xoff := range []int{-1, 0, 1} {
		for _, yoff := range []int{-1, 0, 1} {
			if xoff == 0 && yoff == 0 {
				continue
			}
			if oob(p.x+xoff) || oob(p.y+yoff) {
				continue
			}
			r = append(r, point{p.x + xoff, p.y + yoff})
		}
	}
	return r
}

type state struct{ lvl, inc int }

func distModDim(x, y int) int {
	x %= dim
	y %= dim
	return (dim + x - y) % dim
}

func (s *soln) zero() int          { return -s.steps%dim + dim }
func (s *soln) fromAbs(in int) int { return (in + s.zero()) % dim }
func (s *soln) toAbs(in int) int   { return distModDim(in, s.zero()) }

// func (s *soln) norm(in int) int { return (dim + s.lvl - s.zero()) % dim }

func (s *soln) String() string {
	var b strings.Builder
	for y := 0; y < dim; y++ {
		for x := 0; x < dim; x++ {
			lvl := s.all[point{x, y}].lvl
			b.WriteRune(rune('0' + s.toAbs(lvl)))
		}
		b.WriteRune('\n')
	}
	return b.String()
}

func (s *soln) step() {
	flashPoint := s.fromAbs(dim - 1)
	lib.Dbg("zero: %d, flashPoint: %d\n", s.zero(), flashPoint)
	lib.Dbg("flashing: %v\n", s.byLevel[flashPoint])

	flashed := []point{}
	for len(s.byLevel[flashPoint]) > 0 {
		changed := []point{}
		for f, _ := range s.byLevel[flashPoint] {
			for _, n := range f.neighbors() {
				s.all[n].inc++
				changed = append(changed, n)
			}
			flashed = append(flashed, f)
			delete(s.byLevel[flashPoint], f)
		}
		for _, c := range changed {
			st := s.all[c]
			if st.lvl == flashPoint {
				// We move things into the flashpoint, but not out of it.
				continue
			}
			delete(s.byLevel[st.lvl], c)
			if st.inc > distModDim(flashPoint, st.lvl) {
				st.lvl = flashPoint
			} else {
				*st = state{lvl: (st.lvl + st.inc) % dim}
			}
			s.byLevel[st.lvl][c] = st
		}
		lib.Dbg("%s\n", s)
	}
	for _, f := range flashed {
		st := s.all[f]
		*st = state{lvl: flashPoint}
		s.byLevel[flashPoint][f] = st
	}
	s.steps++
	s.flashes += len(flashed)
	lib.Dbg("%s\n", s)
}

func (s *soln) Part1() int {
	for i := 0; i < 100; i++ {
		s.step()
	}
	return s.flashes
}
func (s *soln) Part2() int { return 0 }

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &soln{all: make(map[point]*state)}
	for i, _ := range r.byLevel {
		r.byLevel[i] = make(map[point]*state)
	}
	y := 0
	for scanner.Scan() {
		if y >= dim {
			return nil, fmt.Errorf("more than %d lines", dim)
		}
		for x, c := range scanner.Text() {
			if x >= dim {
				return nil, fmt.Errorf("line %d more than %d characters", y, dim)
			}
			s := &state{lvl: int(c - '0')}
			p := point{x, y}
			r.byLevel[s.lvl][p] = s
			r.all[p] = s
		}
		y++
	}
	return r, nil
}
