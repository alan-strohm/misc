package day09

import (
	"bufio"
	"fmt"
	"sort"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
	"github.com/mgutz/ansi"
)

type hmap struct {
	depths      grid
	basins      grid
	numBasins   int
	forward     map[int]int
	reverse     map[int][]int
	basinToSize map[int]int
}

var colors = []func(string) string{
	ansi.ColorFunc("black:red"),
	ansi.ColorFunc("black:green"),
	ansi.ColorFunc("black:yellow"),
	ansi.ColorFunc("white:blue"),
	ansi.ColorFunc("white:magenta"),
	ansi.ColorFunc("black:cyan"),
}

func nocolor(in string) string {
	return in
}

func (m *hmap) String() string {
	basinToColor := make(map[int]func(string) string)
	getColor := func(id int) func(string) string {
		if id == invCell {
			return nocolor
		}
		color, ok := basinToColor[id]
		if !ok {
			color = colors[len(basinToColor)%len(colors)]
			basinToColor[id] = color
		}
		return color
	}

	lines := make([]string, len(m.depths))
	for y, row := range m.depths {
		parts := make([]string, 0)
		sameBasin := make([]rune, 0)
		basinID := invCell
		for x, d := range row {
			basin := m.basin(x, y)
			if basin != basinID {
				if len(sameBasin) > 0 {
					parts = append(parts, getColor(basinID)((string(sameBasin))))
				}
				basinID = basin
				sameBasin = sameBasin[:0]
			}
			sameBasin = append(sameBasin, rune('0'+d))
		}
		if len(sameBasin) > 0 {
			parts = append(parts, getColor(basinID)((string(sameBasin))))
		}
		lines[y] = strings.Join(parts, "")
	}
	pois := []struct{ x, y int }{{32, 7}}
	for _, p := range pois {
		if p.y < len(m.depths) {
			lines = append(lines, fmt.Sprintf("%d, %d mapped: %d, unmapped: %d", p.x, p.y, m.basin(p.x, p.y), m.basins[p.y][p.x]))
		}
	}
	return strings.Join(lines, "\n")
}

type dir struct{ xoff, yoff int }

var (
	top    = dir{0, -1}
	right  = dir{1, 0}
	bottom = dir{0, 1}
	left   = dir{-1, 0}
	dirs   = []dir{top, right, bottom, left}
)

const (
	maxDepth = 9
	invCell  = maxDepth + 1
	minBasin = invCell + 1
)

type grid [][]int

func (g grid) neighbor(x, y int, d dir) int {
	nx, ny := x+d.xoff, y+d.yoff
	if ny >= 0 && ny < len(g) && nx >= 0 && nx < len(g[ny]) {
		return g[ny][nx]
	}
	return invCell
}

func (g grid) neighbors(x, y int) []int {
	r := make([]int, len(dirs))
	for i, d := range dirs {
		r[i] = g.neighbor(x, y, d)
	}
	return r
}

func (m *hmap) mapBasin(in int) int {
	if nid, ok := m.forward[in]; ok {
		return nid
	}
	return in
}

func (m *hmap) basin(x, y int) int                { return m.mapBasin(m.basins[y][x]) }
func (m *hmap) neighborBasin(x, y int, d dir) int { return m.mapBasin(m.basins.neighbor(x, y, d)) }

func (m *hmap) setBasin(x, y, b int) {
	m.basins[y][x] = b
	if b != invCell {
		m.basinToSize[b]++
	}
}

func (m *hmap) mergeBasins(from, to int) {
	lib.Dbg("merging %d -> %d\n", from, to)
	m.forward[from] = to
	m.reverse[to] = append(m.reverse[to], from)
	if prevs, ok := m.reverse[from]; ok {
		for _, prev := range prevs {
			m.mergeBasins(prev, to)
		}
		delete(m.reverse, from)
	}
	m.basinToSize[to] += m.basinToSize[from]
	m.basinToSize[from] = 0
}

func (m *hmap) addRow(row []int) {
	y := len(m.depths)
	m.depths = append(m.depths, row)
	m.basins = append(m.basins, make([]int, len(row)))

	topMerged := false // Has the current basin already been merged into a top basin?
	for x, d := range row {
		tb, lb := m.neighborBasin(x, y, top), m.neighborBasin(x, y, left)
		switch {
		case d == maxDepth:
			m.setBasin(x, y, invCell)
			topMerged = false
		case tb < minBasin && lb < minBasin:
			m.setBasin(x, y, minBasin+m.numBasins)
			m.numBasins++
		case tb >= minBasin && lb >= minBasin && tb != lb:
			// We need to merge basins.  By default, the top basin ID wins.
			from, to := lb, tb
			if topMerged {
				// There are multiple top-basins.  The left-most top ID wins.
				from, to = tb, lb
			}
			topMerged = true
			m.mergeBasins(from, to)
			m.setBasin(x, y, to)
		case tb >= minBasin:
			m.setBasin(x, y, tb)
		default:
			m.setBasin(x, y, lb)
		}
	}
	lib.Dbg("%s\n\n", m)
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &hmap{basinToSize: make(map[int]int), forward: make(map[int]int), reverse: make(map[int][]int)}
	for scanner.Scan() {
		row := make([]int, len(scanner.Text()))
		for i, r := range scanner.Text() {
			row[i] = int(r - '0')
		}
		r.addRow(row)
	}
	return r, nil
}

func (m *hmap) Part1() int {
	risk := 0
	for y, row := range m.depths {
		for x, v := range row {
			low := true
			for _, n := range m.depths.neighbors(x, y) {
				if n <= v {
					low = false
				}
			}
			if low {
				lib.Dbg("low point: %d, %d (v: %d, neighbors: %v)\n", x, y, v, m.depths.neighbors(x, y))
				risk += v + 1
			}
		}
	}
	return risk
}

func (m *hmap) Part2() int {
	basinSizes := make([]int, 0, len(m.basinToSize))
	for _, size := range m.basinToSize {
		basinSizes = append(basinSizes, size)
	}
	sort.Ints(basinSizes)
	return basinSizes[len(basinSizes)-1] * basinSizes[len(basinSizes)-2] * basinSizes[len(basinSizes)-3]
}
