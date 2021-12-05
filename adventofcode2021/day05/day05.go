package day05

import (
	"bufio"
	"fmt"
	"math"
)

type point struct {
	x, y float64
}

func (p *point) String() string      { return fmt.Sprintf("%f,%f", p.x, p.y) }
func (p *point) round() *point       { return &point{math.RoundToEven(p.x), math.RoundToEven(p.y)} }
func (p *point) add(o *point) *point { return &point{p.x + o.x, p.y + o.y} }

func (p *point) eq(o *point) bool {
	pr, or := p.round(), o.round()
	return pr.x == or.x && pr.y == or.y
}

// Return a unit vector pointing from p to o.
func (p *point) vecTo(o *point) *point {
	dx, dy := o.x-p.x, o.y-p.y
	d := math.Sqrt(dx*dx + dy*dy)
	return &point{(o.x - p.x) / d, (o.y - p.y) / d}
}

type line struct {
	start, end point
}

func (l *line) diag() bool { return l.start.x != l.end.x && l.start.y != l.end.y }

func (l *line) points() []point {
	v := l.start.vecTo(&l.end)
	r := make([]point, 0, 2)
	for i := &l.start; !l.end.eq(i); i = i.add(v).round() {
		r = append(r, *i)
	}
	return append(r, l.end)
}

func parseLine(s string) (*line, error) {
	r := &line{}
	if _, err := fmt.Sscanf(s, "%f,%f -> %f,%f\n", &r.start.x, &r.start.y, &r.end.x, &r.end.y); err != nil {
		return nil, err
	}
	return r, nil
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	visits := make(map[string]int)
	numTwice := 0
	for scanner.Scan() {
		l, err := parseLine(scanner.Text())
		if err != nil {
			return 0, err
		}
		if l.diag() && p1 {
			continue
		}
		for _, p := range l.points() {
			visits[p.String()]++
			if visits[p.String()] == 2 {
				numTwice++
			}
		}
	}
	return numTwice, nil
}
