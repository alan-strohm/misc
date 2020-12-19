package day17

import (
	"flag"
	"fmt"
	"log"
	"math"
	"strings"

	"../io"
)

var dbg = flag.Bool("dbg", false, "Print debug info.")

type cube rune

const (
	activeCube   cube = '#'
	inactiveCube      = '.'
	unknownCube       = '!'
)

type Hypercube struct {
	lower []*Hypercube
	atoms []cube
	dim   int
}

type point []int

func zero(dim int) point {
	return make([]int, dim)
}

func newHypercube(dim, sideLen int) *Hypercube {
	if dim == 1 {
		r := &Hypercube{atoms: make([]cube, sideLen), dim: dim}
		for i, _ := range r.atoms {
			r.atoms[i] = unknownCube
		}
		return r
	}
	r := &Hypercube{dim: dim, lower: make([]*Hypercube, sideLen)}
	for i, _ := range r.lower {
		r.lower[i] = newHypercube(dim-1, sideLen)
	}
	return r
}

func (h *Hypercube) sideLen() int {
	if h.dim == 1 {
		return len(h.atoms)
	}
	return len(h.lower)
}

// ( 1, 1), ( 1, 0), ( 1,-1}
// ( 0, 1),  				 ( 0,-1)
// (-1, 1), (-1, 0), (-1,-1)
func originNeighbors(dim int) []point {
	if dim == 1 {
		return []point{
			{1}, {-1},
		}
	}
	r := make([]point, 0, int(math.Pow(3, float64(dim)))-1)
	lower := originNeighbors(dim - 1)
	lower = append(lower, zero(dim-1))
	for _, l := range lower {
		for _, i := range []int{-1, 1, 0} {
			r = append(r, append([]int{i}, l...))
		}
	}
	return r[0 : len(r)-1]
}

func (p point) Add(o point) point {
	if len(p) != len(p) {
		log.Fatalf("can not add points of different dimensions: %d vs %d", len(o), len(p))
	}
	r := zero(len(o))
	for i, _ := range p {
		r[i] = p[i] + o[i]
	}
	return r
}

func (p point) neighbors() []point {
	origin := originNeighbors(len(p))
	r := make([]point, len(origin))
	for i, o := range origin {
		r[i] = p.Add(o)
	}
	return r
}

func (h *Hypercube) prevPoint(p point) *point {
	if len(p) != h.dim {
		log.Fatalf("prevPoint: point dim %d != hcube dim %d", len(p), h.dim)
	}
	r := zero(h.dim)
	if h.dim == 0 {
		return &r
	}
	if p[0] <= 0 || p[0] >= h.sideLen()-1 {
		return nil
	}
	r[0] = p[0] - 1
	if h.dim == 1 {
		return &r
	}
	lp := h.lower[0].prevPoint(p[1:])
	if lp == nil {
		return nil
	}
	for i, v := range *lp {
		r[i+1] = v
	}
	return &r
}

func (h *Hypercube) visit(f func(point)) {
	if h.dim == 1 {
		for i, _ := range h.atoms {
			f(point([]int{i}))
		}
		return
	}
	for i, _ := range h.lower {
		h.lower[i].visit(func(p point) {
			f(append([]int{i}, p...))
		})
	}
}

func (h Hypercube) get(p point) cube {
	if len(p) != h.dim {
		log.Fatalf("prevPoint: point dim %d != hcube dim %d", len(p), h.dim)
	}
	if h.dim == 1 {
		return h.atoms[p[0]]
	}
	return h.lower[p[0]].get(p[1:])
}

func (h Hypercube) set(p point, c cube) {
	if len(p) != h.dim {
		log.Fatalf("prevPoint: point dim %d != hcube dim %d", len(p), h.dim)
	}
	if h.dim == 1 {
		h.atoms[p[0]] = c
		return
	}
	h.lower[p[0]].set(p[1:], c)
}

func (h Hypercube) stringWithPrefix(prefix string) string {
	if h.dim == 2 {
		lines := make([]string, len(h.lower))
		for i, sub := range h.lower {
			runes := make([]rune, len(sub.atoms))
			for i, v := range sub.atoms {
				runes[i] = rune(v)
			}
			lines[i] = string(runes)
		}
		return strings.Join(lines, "\n")
	}
	parts := make([]string, 0)
	for i, sub := range h.lower {
		newPrefix := fmt.Sprintf("%d:%d", h.dim, i)
		if prefix != "" {
			newPrefix = prefix + "," + newPrefix
		}
		r := ""
		if sub.dim == 2 {
			r += newPrefix + "\n" + sub.stringWithPrefix("")
		} else {
			r += sub.stringWithPrefix(newPrefix)
		}
		parts = append(parts, r)
	}
	return strings.Join(parts, "\n")
}

func (h Hypercube) String() string {
	return h.stringWithPrefix("")
}

func (h *Hypercube) boot() (numActive int) {
	next := newHypercube(h.dim, h.sideLen()+2)
	next.visit(func(p point) {
		next.set(p, inactiveCube)
		neighbors := p.neighbors()
		activeNeigbors := make([]point, 0)
		activeNeigborsPrev := make([]point, 0)
		for _, n := range neighbors {
			prev := next.prevPoint(n)
			if prev != nil && h.get(*prev) == activeCube {
				activeNeigbors = append(activeNeigbors, n)
				activeNeigborsPrev = append(activeNeigborsPrev, *prev)
			}
		}
		numActiveNeighbors := len(activeNeigbors)
		prev := next.prevPoint(p)
		if prev != nil && h.get(*prev) == activeCube {
			if numActiveNeighbors == 2 || numActiveNeighbors == 3 {
				if *dbg {
					fmt.Printf("%v active: prev: %v active, active neighbors: %v, prev: %v\n", p, prev, activeNeigbors, activeNeigborsPrev)
				}
				next.set(p, activeCube)
				numActive++
			}
		}
		if (prev == nil || h.get(*prev) == inactiveCube) && numActiveNeighbors == 3 {
			if *dbg {
				fmt.Printf("%v active: prev: %v inactive, active neighbors: %v, prev: %v\n", p, prev, activeNeigbors, activeNeigborsPrev)
			}
			next.set(p, activeCube)
			numActive++
		}
	})
	*h = *next
	return
}

type scanner struct {
	in [][]cube
}

func (s *scanner) hypercube(dim int) *Hypercube {
	r := newHypercube(dim, len(s.in))
	r.visit(func(p point) {
		for _, v := range p[:len(p)-2] {
			if v != len(s.in)/2 {
				r.set(p, inactiveCube)
				return
			}
		}
		y, x := p[len(p)-2], p[len(p)-1]
		r.set(p, s.in[y][x])
	})
	return r
}

func (s *scanner) ScanLine(l string) (bool, error) {
	runes := []rune(l)
	if len(s.in) > 0 && len(s.in[0]) != len(runes) {
		return false, fmt.Errorf("uneven input: len(line1) = %d, line(currLine) = %d", len(s.in[0]), len(runes))
	}
	s.in = append(s.in, make([]cube, len(runes)))
	row := &s.in[len(s.in)-1]
	for i, r := range runes {
		if cube(r) != activeCube && cube(r) != inactiveCube {
			return false, fmt.Errorf("unexpected cube: %s", string(r))
		}
		(*row)[i] = cube(r)
	}
	return false, nil
}

func NumActiveCubes(fname string, dim, boots int) (numActive int, err error) {
	s := &scanner{in: make([][]cube, 0)}
	if err = io.Scan(s, fname); err != nil {
		return -1, err
	}
	cube := s.hypercube(dim)
	if *dbg {
		fmt.Println("Initialized:")
		fmt.Println(cube)
		fmt.Println("Booting...")
	}
	for i := 0; i < boots; i++ {
		numActive = cube.boot()
		if *dbg {
			fmt.Println(cube)
		}
	}
	return numActive, nil
}
