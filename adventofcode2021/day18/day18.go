package day18

import (
	"bufio"
	"fmt"
	"math"
	"math/bits"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

const maxDepth = 3

type cell struct {
	num     int
	present bool
}

type number struct {
	cells [16]cell
}

func parse(in string) *number {
	r := &number{}
	idx := 0
	loc := maxDepth + 1
	for _, c := range []byte(in) {
		switch c {
		case '[':
			loc--
			idx &= ^(1 << loc) // Set loc in id to zero
		case ',':
			idx |= (1 << loc) // Set loc in id to one
		case ']':
			idx &= ^(1 << loc) // Set loc in id to zero
			loc++
		default:
			r.cells[idx] = cell{num: int(c - '0'), present: true}
		}
	}
	return r
}

type pair struct {
	num  *number
	l, r int
}

func fromNum(num *number) *pair {
	return &pair{num: num, l: 0, r: len(num.cells) / 2}
}
func (p *pair) isRegular() bool { return p.l == p.r || !p.num.cells[p.r].present }
func (p *pair) regular() int    { return p.num.cells[p.l].num }
func (p *pair) left() *pair {
	return &pair{num: p.num, l: p.l, r: (p.l + p.r) / 2}
}
func (p *pair) right() *pair {
	return &pair{num: p.num, l: p.r, r: p.r + (p.r-p.l)/2}
}

func (n *number) String() string {
	var rec func(p *pair) string
	rec = func(p *pair) string {
		if p.isRegular() {
			return string([]byte{byte('0' + p.regular())})
		}
		return fmt.Sprintf("[%s,%s]", rec(p.left()), rec(p.right()))
	}
	return rec(fromNum(n))
}

func mergeAndExplode(a, b *number) *number {
	r := &number{}
	carry := 0
	for i := 0; i < len(a.cells)+len(b.cells); i++ {
		ni := i >> 1
		var c *cell
		if i < len(a.cells) {
			c = &a.cells[i]
		} else {
			c = &b.cells[i-len(a.cells)]
			ni |= (1 << maxDepth)
		}

		if !c.present {
			continue
		}

		if i&1 == 1 { // Explode
			r.addToNeighbor(ni, -1, r.cells[ni].num)
			r.cells[ni].num = 0
			carry = c.num
			continue
		}

		num := c.num
		if carry != 0 {
			num += carry
			carry = 0
		}

		r.cells[ni] = cell{num, true}
	}
	return r
}

func (n *number) splitAndExplode() bool {
	for i, c := range n.cells {
		if c.num < 10 {
			continue
		}
		l, r := c.num/2, ceil(c.num, 2)
		sib := n.nextSibling(i)

		if sib == -1 { // Explode
			n.addToNeighbor(i, -1, l)
			n.addToNeighbor(i, 1, r)
			n.cells[i].num = 0
		} else {
			n.cells[i].num = l
			n.cells[sib] = cell{r, true}
		}
		return true
	}
	return false
}

func (n *number) nextSibling(x int) int {
	zeros := bits.TrailingZeros(uint(x))
	if x == 0 {
		zeros = 4
	}
	if zeros == 0 {
		return -1
	}
	for i := 1 << (zeros - 1); i > 0; i >>= 1 {
		if !n.cells[x+i].present {
			return x + i
		}
	}
	return -1
}

func (n *number) addToNeighbor(start, dir, val int) {
	for i := start + dir; i < len(n.cells) && i >= 0; i += dir {
		if n.cells[i].present {
			n.cells[i].num += val
			break
		}
	}
}

func (n *number) add(o *number) *number {
	r := mergeAndExplode(n, o)
	for r.splitAndExplode() {
	}
	return r
}

func (n *number) magnitude() int {
	var rec func(p *pair) int
	rec = func(p *pair) int {
		if p.isRegular() {
			return p.regular()
		}
		return 3*rec(p.left()) + 2*rec(p.right())
	}
	return rec(fromNum(n))
}

func ceil(num, div int) int {
	return int(math.Ceil(float64(num) / float64(div)))
}

type hw []*number

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &hw{}
	for scanner.Scan() {
		*r = append(*r, parse(scanner.Text()))
	}
	return r, nil
}

func (h *hw) Part1() int {
	sum := (*h)[0]
	for _, n := range (*h)[1:] {
		sum = sum.add(n)
	}
	return sum.magnitude()
}

func (h *hw) Part2() int {
	max := 0
	for i, a := range *h {
		for j, b := range *h {
			if i != j {
				if mag := a.add(b).magnitude(); mag > max {
					max = mag
				}
			}
		}
	}
	return max
}
