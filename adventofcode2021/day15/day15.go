package day15

import (
	"bufio"
	"container/heap"
	"fmt"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
	"github.com/mgutz/ansi"
)

type point struct{ x, y int }

func (a point) add(b point) point { return point{a.x + b.x, a.y + b.y} }

type cell struct {
	pos        point
	qidx       int
	risk, dist int
	done       bool
	prev       *cell
}

type cellQueue []*cell

func (q cellQueue) Len() int           { return len(q) }
func (q cellQueue) Less(i, j int) bool { return q[i].dist < q[j].dist }
func (q cellQueue) Swap(i, j int) {
	q[i], q[j] = q[j], q[i]
	q[i].qidx, q[j].qidx = i, j
}

func (q *cellQueue) Pop() interface{} {
	r := (*q)[len(*q)-1]
	r.qidx = -1
	*q = (*q)[0 : len(*q)-1]
	return r
}

func (q *cellQueue) Push(x interface{}) {
	it := x.(*cell)
	it.qidx = len(*q)
	*q = append(*q, it)
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
				fmt.Fprintf(&sb, "%4c", '*')
			} else {
				fmt.Fprintf(&sb, "%4d", c.dist)
			}
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (c *cavern) shortestPath(start, end *cell) int {
	q := &cellQueue{}
	heap.Push(q, start)
	visited := map[point]bool{}
	for len(*q) > 0 {
		next := heap.Pop(q).(*cell)
		visited[next.pos] = true
		for _, nbor := range c.neighbors(next) {
			if visited[nbor.pos] {
				continue
			}
			if nbor.dist == maxInt {
				heap.Push(q, nbor)
			}
			alt := next.dist + nbor.risk
			if alt < nbor.dist {
				nbor.prev = next
				nbor.dist = alt
				heap.Fix(q, nbor.qidx)
			}
		}
	}

	lib.Dbg("%s\n", c)
	return end.dist
}

func (c *cavern) Part1() int {
	return c.shortestPath(c.cells[0][0], c.cells[c.maxY][c.maxX])
}

func (c *cavern) Part2() int {
	oldh, oldw := c.maxY+1, c.maxX+1
	h, w := oldh*5, oldw*5
	cells := make([][]*cell, h)
	for y, _ := range cells {
		cells[y] = make([]*cell, w)
		for x, _ := range cells[y] {
			oldx, oldy := x%oldw, y%oldh
			oldrisk := c.cells[oldy][oldx].risk
			risk := oldrisk + (x-oldx)/oldw + (y-oldy)/oldh
			if risk > 9 {
				risk = (risk%10 + 1)
			}
			cells[y][x] = &cell{pos: point{x, y}, risk: risk, dist: maxInt}
		}
	}
	cells[0][0].dist = 0

	c.cells = cells
	c.maxX, c.maxY = w-1, h-1
	return c.shortestPath(c.cells[0][0], c.cells[c.maxY][c.maxX])
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
