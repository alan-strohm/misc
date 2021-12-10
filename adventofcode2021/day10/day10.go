package day10

import (
	"bufio"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type checker struct {
	p1score int
	lineNum int
}

func (c *checker) Part1() int {
	return c.p1score
}

func (c *checker) Part2() int {
	return 0
}

var (
	chunks = map[rune]rune{
		'(': ')', '{': '}', '[': ']', '<': '>',
	}

	scores = map[rune]int{
		')': 3, ']': 57, '}': 1197, '>': 25137,
	}
)

func (c *checker) checkLine(in string) {
	c.lineNum++
	stack := make([]rune, 0)
	for i, r := range in {
		t := len(stack) - 1
		if e, ok := chunks[r]; ok {
			stack = append(stack, e)
		} else if r == stack[t] {
			stack = stack[0:t]
		} else {
			lib.Dbg("line %d, pos: %d: expected %c, but found %c instead\n", c.lineNum, i, stack[t], r)
			c.p1score += scores[r]
			return
		}
	}
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &checker{}
	for scanner.Scan() {
		r.checkLine(scanner.Text())
	}
	return r, nil
}
