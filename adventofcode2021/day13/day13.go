package day13

import (
	"bufio"
	"fmt"
	"sort"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type point struct{ x, y int }

func (a point) add(b point) point { return point{a.x + b.x, a.y + b.y} }
func (a point) scale(s int) point { return point{s * a.x, s * a.y} }
func (a point) dot(b point) int   { return a.x*b.x + a.y*b.y }
func (a point) sub(b point) point { return a.add(b.scale(-1)) }
func (a point) lt(b point) bool {
	if a.y != b.y {
		return a.y < b.y
	}
	return a.x < b.x
}

type line struct{ a, n point }

func (l line) nearestTo(p point) point {
	projDist := l.n.dot(p.sub(l.a))
	return l.a.add(l.n.scale(projDist))
}

type manual struct {
	points []point
	folds  []line
}

func toString(in map[point]bool) string {
	all := make([]point, 0, len(in))
	for p, _ := range in {
		all = append(all, p)
	}
	sort.Slice(all, func(i, j int) bool { return all[i].lt(all[j]) })
	lib.Dbg("%v\n", all)
	i := point{0, 0}
	var sb strings.Builder
	for _, p := range all {
		if i.y < p.y {
			sb.WriteString(strings.Repeat("\n", p.y-i.y))
		}
		if i.x+1 < p.x {
			sb.WriteString(strings.Repeat(" ", p.x-i.x-1))
		}
		i = p
		sb.WriteRune('#')
	}
	return sb.String()
}

func (m *manual) fold() int {
	r := map[point]bool{}
	for _, p := range m.points {
		mapped := p
		for _, f := range m.folds {
			nearest := f.nearestTo(mapped)
			if mapped.lt(nearest) {
				continue
			}
			mapped = nearest.sub(mapped.sub(nearest))
		}
		r[mapped] = true
	}
	lib.Dbg("%s\n", toString(r))
	return len(r)
}

func New(scanner *bufio.Scanner, p1 bool) (*manual, error) {
	r := &manual{}
	points := true
	for scanner.Scan() {
		switch {
		case scanner.Text() == "":
			points = false
		case points:
			var x, y int
			if _, err := fmt.Sscanf(scanner.Text(), "%d,%d", &x, &y); err != nil {
				return nil, err
			}
			r.points = append(r.points, point{x, y})
		default:
			var dir rune
			var val int
			if _, err := fmt.Sscanf(scanner.Text(), "fold along %c=%d", &dir, &val); err != nil {
				return nil, err
			}
			if dir == 'x' {
				r.folds = append(r.folds, line{a: point{val, 0}, n: point{0, 1}})
			} else {
				r.folds = append(r.folds, line{a: point{0, val}, n: point{1, 0}})
			}
			if p1 {
				return r, nil
			}
		}
	}
	return r, nil
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	if r, err := New(scanner, p1); err != nil {
		return 0, nil
	} else {
		return r.fold(), nil
	}
}
