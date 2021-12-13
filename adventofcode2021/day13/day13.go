package day13

import (
	"bufio"
	"fmt"
	"sort"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type point struct{ x, y int }

type manual struct {
	points []point
	xfolds []int
	yfolds []int
}

func toString(in map[point]bool) string {
	all := make([]point, 0, len(in))
	for p, _ := range in {
		all = append(all, p)
	}
	sort.Slice(all, func(i, j int) bool {
		if all[i].y != all[j].y {
			return all[i].y < all[j].y
		}
		return all[i].x < all[j].x
	})
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

func fold(in int, folds []int) int {
	for _, f := range folds {
		if in > f {
			in = f - (in - f)
		}
	}
	return in
}

func (m *manual) fold() int {
	r := map[point]bool{}
	for _, p := range m.points {
		r[point{x: fold(p.x, m.xfolds), y: fold(p.y, m.yfolds)}] = true
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
				r.xfolds = append(r.xfolds, val)
			} else {
				r.yfolds = append(r.yfolds, val)
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
