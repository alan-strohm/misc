package day25

import (
	"bufio"
	"fmt"
	"math"
)

func check(cond bool, format string, a ...interface{}) {
	if !cond {
		panic(fmt.Sprintf(format, a...))
	}
}

type point struct{ x, y int }

type cuc struct {
	kind byte
	pos  point
}

func (c *cuc) Format(f fmt.State, verb rune) {
	fmt.Fprintf(f, "%c:%v", c.kind, c.pos)
}

type state struct {
	poss      map[point]*cuc
	unblocked map[byte][]*cuc
	h, w      int
}

func (s *state) Format(f fmt.State, verb rune) {
	for y := 0; y < s.h; y++ {
		for x := 0; x < s.w; x++ {
			c := s.poss[point{x, y}]
			if c == nil {
				f.Write([]byte{'.'})
			} else {
				f.Write([]byte{c.kind})
			}
		}
		f.Write([]byte{'\n'})
	}
}

// The next cuc waiting to get into c's spot, if any.
func (s *state) nextCuc(c *cuc) *cuc {
	westp, northp := c.pos, c.pos
	northp.y--
	if northp.y < 0 {
		northp.y += s.h
	}
	westp.x--
	if westp.x < 0 {
		westp.x += s.w
	}
	west, north := s.poss[westp], s.poss[northp]
	if west != nil && west.kind != '>' {
		west = nil
	}
	if north != nil && north.kind != 'v' {
		north = nil
	}
	order := []*cuc{west, north}
	if c.kind == '>' {
		order = []*cuc{north, west}
	}
	for _, c := range order {
		if c != nil {
			return c
		}
	}
	return nil
}

// The next position for c.
func (s *state) nextPos(c *cuc) point {
	if c.kind == 'v' {
		return point{c.pos.x, (c.pos.y + 1) % s.h}
	}
	return point{(c.pos.x + 1) % s.w, c.pos.y}
}

func (s *state) enqueue(c *cuc) {
	s.unblocked[c.kind] = append(s.unblocked[c.kind], c)
}

func (s *state) step(kind byte) bool {
	todo, unblocked := []*cuc{}, []*cuc{}
	todo, s.unblocked[kind] = s.unblocked[kind], todo
	moved := map[*cuc]bool{}
	for _, c := range todo {
		if s.poss[s.nextPos(c)] == nil && !moved[c] {
			unblocked = append(unblocked, c)
			moved[c] = true
		}
	}
	for _, c := range unblocked {
		nextCuc := s.nextCuc(c)
		if nextCuc != nil {
			s.enqueue(nextCuc)
		}
		nextPos := s.nextPos(c)
		check(s.poss[nextPos] == nil, "attempt to move into already occupied position: %v from: %v", s.poss[nextPos], c)
		s.poss[nextPos] = c
		delete(s.poss, c.pos)
		c.pos = nextPos
	}
	for _, c := range unblocked {
		if s.poss[s.nextPos(c)] == nil {
			s.enqueue(c)
		}
	}
	return len(unblocked) > 0
}

func (s *state) run(until int) int {
	steps := 0
	for movedOne := true; movedOne && steps < until; steps++ {
		east, south := s.step('>'), s.step('v')
		movedOne = east || south
	}
	return steps
}

func parse(scanner *bufio.Scanner) *state {
	r := &state{poss: map[point]*cuc{}, unblocked: map[byte][]*cuc{}}
	y := 0
	for ; scanner.Scan(); y++ {
		r.w = len(scanner.Text())
		for x, k := range []byte(scanner.Text()) {
			if k == '.' {
				continue
			}
			p := point{x, y}
			r.poss[p] = &cuc{kind: k, pos: p}
		}
	}
	r.h = y
	for _, c := range r.poss {
		if r.poss[r.nextPos(c)] == nil {
			r.enqueue(c)
		}
	}
	return r
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	return parse(scanner).run(math.MaxInt), nil
}
