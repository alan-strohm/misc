package day02

import (
	"bufio"
	"fmt"
	"strconv"
	"strings"
)

type sub interface {
	forward(units int)
	down(units int)
	depth() int
	pos() int
}

type p1 struct {
	d, p int
}

func (p *p1) forward(units int) { p.p += units }
func (p *p1) down(units int)    { p.d += units }
func (p *p1) depth() int        { return p.d }
func (p *p1) pos() int          { return p.p }

type p2 struct {
	d, p, aim int
}

func (p *p2) forward(units int) {
	p.p += units
	p.d += units * p.aim
}
func (p *p2) down(units int) { p.aim += units }
func (p *p2) depth() int     { return p.d }
func (p *p2) pos() int       { return p.p }

func Run(scanner *bufio.Scanner, part1 bool) (int, error) {
	var s sub = &p1{}
	if !part1 {
		s = &p2{}
	}
	for i := 0; scanner.Scan(); i++ {
		parts := strings.Split(scanner.Text(), " ")
		arg, err := strconv.Atoi(parts[1])
		if err != nil {
			return 0, fmt.Errorf("invalid line [%d]: %s", i, scanner.Text())
		}
		switch parts[0] {
		case "forward":
			s.forward(arg)
		case "down":
			s.down(arg)
		case "up":
			s.down(-arg)
		}
	}
	return s.pos() * s.depth(), nil
}
