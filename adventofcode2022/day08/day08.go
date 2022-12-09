package day08

import (
	"bufio"
	"math"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

type vec struct{ x, y int }

func (v vec) add(o vec) vec {
	return vec{x: v.x + o.x, y: v.y + o.y}
}

type cell struct {
	height int8
	maxs   [2][2]int8
}

func (c *cell) visible() bool {
	for y, row := range c.maxs {
		for x, max := range row {
			if max < c.height {
				lib.Dbg("%d%c ", max, dirNames[y][x])
				return true
			}
		}
	}
	return false
}

type grid [][]cell

func (g grid) contains(p vec) bool {
	return p.x >= 0 && p.y >= 0 && p.x < len(g[0]) && p.y < len(g)
}

func (g grid) get(p vec) *cell {
	return &g[p.y][p.x]
}

var dirs = [2][2]vec{
	{{x: 0, y: 1}, {x: 1, y: 0}},
	{{x: 0, y: -1}, {x: -1, y: 0}},
}

const (
	majorDir = 0
	minorDir = 1
)

var dirNames = [2][2]rune{{'S', 'E'}, {'N', 'W'}}

func (g grid) fillMaxes() {
	lib.Dbg("Filling maxes...\n")
	// Traverse the grid from the upper left to the lower right and then return
	// in reverse order.
	p := vec{x: 0, y: 0}
	for y, fwd := range dirs {
		revY := (y + 1) % len(dirs)
		rev := dirs[revY]
		lib.Dbg("Rev: %v\n", rev)
		for start := p; g.contains(p); p = p.add(fwd[majorDir]) {
			for p.x = start.x; g.contains(p); p = p.add(fwd[minorDir]) {
				// Keep a running count of the max height seen so far in the opposite
				// direction of the direction of travel.
				c := g.get(p)
				for x, dir := range rev {
					prevP := p.add(dir)
					if !g.contains(prevP) {
						c.maxs[revY][x] = -1
					} else {
						prevC := g.get(prevP)
						c.maxs[revY][x] = int8(math.Max(float64(prevC.maxs[revY][x]), float64(prevC.height)))
					}
					lib.Dbg("%d%c", c.maxs[revY][x], dirNames[revY][x])
				}
				lib.Dbg(" ")
			}
			// Usually we'll reset x to start.x on the next loop.  This allows
			// g.contains() to succeed and also means that at the end of the outer
			// loop we're at the lower-right corner for the reverse pass.
			p = p.add(rev[minorDir])
			lib.Dbg("\n")
		}
		p = p.add(rev[majorDir])
		lib.Dbg("\n\n")
	}
}

func parseGrid(s *bufio.Scanner) (r grid) {
	for s.Scan() {
		row := make([]cell, len(s.Text()))
		for i, b := range []byte(s.Text()) {
			row[i].height = int8(b - '0')
		}
		r = append(r, row)
	}
	r.fillMaxes()
	return
}

func (g grid) countVisible() (visible int) {
	lib.Dbg("Counting visible...\n")
	for _, row := range g {
		for _, cell := range row {
			if cell.visible() {
				visible++
			}
		}
		lib.Dbg("\n")
	}
	return
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	g := parseGrid(s)
	if isPart1 {
		return g.countVisible(), nil
	}
	return 0, nil
}
