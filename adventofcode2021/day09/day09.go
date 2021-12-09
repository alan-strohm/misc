package day09

import (
	"bufio"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type hmap struct {
	grid [][]int
}

func (m *hmap) neighbors(x, y int) []int {
	offs := []int{0, 1, 0, -1}
	r := make([]int, len(offs))
	for i, xoff := range offs {
		yoff := offs[(i+1)%len(offs)]
		nx, ny := x+xoff, y+yoff
		if ny >= 0 && ny < len(m.grid) && nx >= 0 && nx < len(m.grid[ny]) {
			r[i] = m.grid[ny][nx]
		} else {
			r[i] = 10
		}
	}
	return r
}

func (m *hmap) Part1() int {
	risk := 0
	for y, row := range m.grid {
		for x, v := range row {
			low := true
			for _, n := range m.neighbors(x, y) {
				if n <= v {
					low = false
				}
			}
			if low {
				lib.Dbg("low point: %d, %d (v: %d, neighbors: %v)\n", x, y, v, m.neighbors(x, y))
				risk += v + 1
			}
		}
	}
	return risk
}

func (m *hmap) Part2() int {
	return 0
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := hmap{}
	for scanner.Scan() {
		row := make([]int, len(scanner.Text()))
		for i, r := range scanner.Text() {
			row[i] = int(r - '0')
		}
		r.grid = append(r.grid, row)
	}
	return &r, nil
}
