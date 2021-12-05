package day05

import (
	"bufio"
	"fmt"
	"math"
)

type point struct {
	x, y int
}
type vec struct {
	x, y float64
}

func (p *point) String() string   { return fmt.Sprintf("%d,%d", p.x, p.y) }
func (p *point) eq(o *point) bool { return p.x == o.x && p.y == o.y }
func (p *point) toVec() *vec      { return &vec{x: float64(p.x), y: float64(p.y)} }

func fromVec(v *vec) *point {
	return &point{x: int(math.RoundToEven(v.x)), y: int(math.RoundToEven(v.y))}
}

// Return a unit vector pointing from p to o.
func (p *point) vecTo(o *point) *vec {
	d := math.Sqrt(float64((o.x-p.x)*(o.x-p.x) + (o.y-p.y)*(o.y-p.y)))
	return &vec{x: float64(o.x-p.x) / d, y: float64(o.y-p.y) / d}
}

func (v *vec) add(o *vec) *vec { return &vec{x: v.x + o.x, y: v.y + o.y} }

type line struct {
	start, end point
}

func (l *line) diag() bool { return l.start.x != l.end.x && l.start.y != l.end.y }

func (l *line) points() []point {
	v := l.start.vecTo(&l.end)
	r := make([]point, 0, 2)
	for i := l.start.toVec(); ; {
		p := fromVec(i)
		r = append(r, *p)
		if p.eq(&l.end) {
			break
		}
		i = p.toVec().add(v)
	}
	return r
}

func parseLine(s string) (*line, error) {
	r := &line{}
	if _, err := fmt.Sscanf(s, "%d,%d -> %d,%d\n", &r.start.x, &r.start.y, &r.end.x, &r.end.y); err != nil {
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
