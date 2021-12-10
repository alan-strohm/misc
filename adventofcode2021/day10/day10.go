package day10

import (
	"bufio"
	"sort"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type checker struct {
	errScore   int
	cmplScores []int
	lineNum    int
}

func (c *checker) Part1() int {
	return c.errScore
}

func (c *checker) Part2() int {
	sort.Ints(c.cmplScores)
	lib.Dbg("scores: %v\n", c.cmplScores)
	return c.cmplScores[len(c.cmplScores)/2]
}

var (
	chunks = map[rune]rune{
		'(': ')', '{': '}', '[': ']', '<': '>',
	}

	errScores = map[rune]int{
		')': 3, ']': 57, '}': 1197, '>': 25137,
	}

	cmplScores = map[rune]int{
		')': 1, ']': 2, '}': 3, '>': 4,
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
			c.errScore += errScores[r]
			return
		}
	}
	cmplScore := 0
	lib.Dbg("line %d, completion: ", c.lineNum)
	for i := len(stack) - 1; i >= 0; i-- {
		lib.Dbg("%c", stack[i])
		cmplScore *= 5
		cmplScore += cmplScores[stack[i]]
	}
	lib.Dbg(", score: %d\n", cmplScore)
	c.cmplScores = append(c.cmplScores, cmplScore)
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &checker{}
	for scanner.Scan() {
		r.checkLine(scanner.Text())
	}
	return r, nil
}
