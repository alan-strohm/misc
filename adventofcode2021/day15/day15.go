package day15

import (
	"bufio"
	"fmt"
	"sort"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
	"github.com/mgutz/ansi"
)

type point struct{ x, y int }

func (a point) add(b point) point { return point{a.x + b.x, a.y + b.y} }

type cell struct {
	pos        point
	risk, dist int
	done       bool
	prev       *cell
}

type cellQueue []*cell

func (q cellQueue) Len() int           { return len(q) }
func (q cellQueue) Less(i, j int) bool { return q[i].dist < q[j].dist }
func (q cellQueue) Swap(i, j int)      { q[i], q[j] = q[j], q[i] }

func (q *cellQueue) pop() *cell {
	sort.Sort(*q)
	r := (*q)[0]
	*q = (*q)[1:len(*q)]
	return r
}

func (q *cellQueue) push(cells ...*cell) {
	*q = append(*q, cells...)
}

func (q *cellQueue) String() string {
	parts := make([]string, len(*q))
	for i, c := range *q {
		parts[i] = fmt.Sprintf("(%v: %d)", c.pos, c.dist)
	}
	return strings.Join(parts, ",")
}

type cavern struct {
	cells      [][]*cell
	maxX, maxY int
}

func (c *cavern) oob(p point) bool {
	return p.x < 0 || p.x > c.maxX || p.y < 0 || p.y > c.maxY
}

func (c *cavern) neighbors(from *cell) []*cell {
	r := []*cell{}
	for _, off := range []point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
		npos := from.pos.add(off)
		if c.oob(npos) {
			continue
		}
		n := c.cells[npos.y][npos.x]
		if !n.done {
			r = append(r, n)
		}
	}
	return r
}

var red = ansi.ColorFunc("black:red")

func (ca *cavern) String() string {
	var sb strings.Builder
	start, end := ca.cells[0][0], ca.cells[ca.maxY][ca.maxX]
	onShortest := map[point]bool{start.pos: true}
	for i := end; i != start; i = i.prev {
		onShortest[i.pos] = true
	}
	fmt.Fprintf(&sb, "path + risks:\n")
	for _, row := range ca.cells {
		for _, c := range row {
			b := byte('0' + c.risk)
			if onShortest[c.pos] {
				sb.WriteString(red(string([]byte{b})))
			} else {
				sb.WriteByte(b)
			}
		}
		sb.WriteRune('\n')
	}
	fmt.Fprintf(&sb, "\ndists:\n")
	for _, row := range ca.cells {
		for _, c := range row {
			if c.dist == maxInt {
				fmt.Fprintf(&sb, "%3c", '*')
			} else {
				fmt.Fprintf(&sb, "%3d", c.dist)
			}
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (c *cavern) Part1() int {
	q := &cellQueue{}
	start := c.cells[0][0]
	q.push(start)
	for len(*q) > 0 {
		next := q.pop()
		next.done = true
		for _, nbor := range c.neighbors(next) {
			if nbor.dist == maxInt {
				q.push(nbor)
			}
			alt := next.dist + nbor.risk
			if alt < nbor.dist {
				nbor.prev = next
				nbor.dist = alt
			}
		}
	}

	lib.Dbg("%s\n", c)
	end := c.cells[c.maxY][c.maxX]
	return end.dist
}

func (c *cavern) Part2() int {
	return 0
}

const maxInt = int(^uint(0) >> 1)

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &cavern{}
	for scanner.Scan() {
		y := len(r.cells)
		r.cells = append(r.cells, make([]*cell, len(scanner.Text())))
		for x, c := range []byte(scanner.Text()) {
			r.cells[y][x] = &cell{pos: point{x, y}, risk: int(c - '0'), dist: maxInt}
		}
	}
	r.cells[0][0].dist = 0
	r.maxY = len(r.cells) - 1
	r.maxX = len(r.cells[r.maxY]) - 1
	return r, nil
}
