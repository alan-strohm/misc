package day01

import (
	"bufio"
	"container/list"
	"strconv"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func maxBufLen(part1 bool) int {
	if part1 {
		return 1
	}
	return 3
}

func Run(scanner *bufio.Scanner, part1 bool) (int, error) {
	numInc, prev := 0, 0
	buf := list.New()

	for scanner.Scan() {
		depth, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return 0, err
		}
		buf.PushBack(depth)
		if buf.Len() <= maxBufLen(part1) {
			prev += depth
			continue
		}
		front := buf.Front()
		next := prev + depth - front.Value.(int)
		lib.Dbg("%d = %d + %d - %d\n", next, prev, depth, front.Value.(int))
		if next > prev {
			numInc++
		}
		prev = next
		buf.Remove(front)
	}
	return numInc, nil
}
