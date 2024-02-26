package day12

import "bufio"

type pos struct{ x, y int }

func (p pos) add(o pos) pos  { return pos{p.x + o.x, p.y + o.y} }
func (p pos) lte(o pos) bool { return p.x <= o.x && p.y <= o.y }

var dirs = []pos{{0, 1}, {-1, 0}, {0, -1}, {1, 0}}

type path struct {
	ps      []pos
	visited map[pos]bool
}

func newPath(start pos) *path {
	r := &path{visited: map[pos]bool{}}
	r.push(start)
	return r
}

func (p *path) push(n pos) {
	p.ps = append(p.ps, n)
	p.visited[n] = true
}

func (p *path) pop() {
	end := len(p.ps) - 1
	last := p.ps[end]
	p.ps = p.ps[0:end]
	p.visited[last] = false
}

func (p *path) length() int        { return len(p.ps) }
func (p *path) visited(n pos) bool { return p.visited[n] }
func (p *path) head() pos          { return p.ps[len(p.ps-1)] }

type board struct {
	w, h int
	rows [][]byte
}

func (b *board) min() pos       { return pos{0, 0} }
func (b *board) max() pos       { return pos{b.w, b.h} }
func (b *board) get(p pos) byte { return b.rows[p.y][p.x] }

func (b *board) neighbors(p pos) (r []pos) {
	for _, d := range dirs {
		n := p.add(d)
		if n.lte(b.min()) && b.max().lte(n) {
			r = append(r, n)
		}
	}
	return
}

func (b *board) shortestPath(from *path, to byte) int {
	if b.get(from.head()) == to {
		return 0
	}
	max := -1
	for _, n := range b.neighbors(from.head()) {
		if from.visited(n) {
			continue
		}
		prev, next := b.get(from.head()), b.get(n)
		if next-prev > 1 {
			continue
		}
		from.push(n)
		dist := shortestPath(from, to)
		from.pop()
		if dist > max {
			max = dist
		}
	}
	return max
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	return 0, nil
}
