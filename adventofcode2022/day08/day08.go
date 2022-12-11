package day08

import (
	"bufio"
	"math"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

type vec struct{ x, y int }

func (v vec) add(o vec) vec   { return vec{x: v.x + o.x, y: v.y + o.y} }
func (v vec) scale(s int) vec { return vec{x: v.x * s, y: v.y * s} }

type cell struct {
	height  int8
	visible bool
	score   int
}

type grid [][]cell

func (g grid) contains(p vec) bool {
	return p.x >= 0 && p.y >= 0 && p.x < len(g[0]) && p.y < len(g)
}

func (g grid) get(p vec) *cell {
	return &g[p.y][p.x]
}

var (
	north = vec{x: 0, y: -1}
	south = north.scale(-1)
	east  = vec{x: 1, y: 0}
	west  = east.scale(-1)
)

// Fill one line of cell info.  The line is either a row or a column in either
// forwards or reverse order.
func fillLine(line []*cell) {
	maxHeight := float64(-1)
	trees := [10]int{} // Number of trees visible at this position from each of the 10 heights.
	for _, c := range line {
		c.score *= trees[c.height]
		if float64(c.height) > maxHeight {
			c.visible = true
		}
		maxHeight = math.Max(maxHeight, float64(c.height))
		for h, _ := range trees {
			if int8(h) > c.height {
				trees[h]++
			} else {
				trees[h] = 1
			}
		}
	}
}

func (g grid) fill() {
	// Traverse each row forwards and backwards. Then traverse each column forwards and backwards.
	majors := []vec{south, east} // Each row going south, then each column going east.
	// Rows are traversed east and then back west.  Columns are traversed first south and then norht.
	minors := [2][2]vec{{east, west}, {south, north}}
	for i, major := range majors {
		p := vec{x: 0, y: 0}
		for ; g.contains(p); p = p.add(major) {
			for _, minor := range minors[i] {
				line := []*cell{}
				for ; g.contains(p); p = p.add(minor) {
					line = append(line, g.get(p))
				}
				fillLine(line)
				p = p.add(minor.scale(-1)) // Get back on the grid.
				lib.Dbg(" ")
			}
			lib.Dbg("\n")
		}
		lib.Dbg("\n")
	}
}

func (g grid) solve() (numVisible, maxScore int) {
	for _, row := range g {
		for _, c := range row {
			if c.score > maxScore {
				maxScore = c.score
			}
			if c.visible {
				numVisible++
			}
		}
	}
	return
}

func parseGrid(s *bufio.Scanner) (r grid) {
	for s.Scan() {
		row := make([]cell, len(s.Text()))
		for i, b := range []byte(s.Text()) {
			row[i] = cell{height: int8(b - '0'), score: 1}
		}
		r = append(r, row)
	}
	r.fill()
	return
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	numVisible, maxScore := parseGrid(s).solve()
	if isPart1 {
		return numVisible, nil
	}
	return maxScore, nil
}
