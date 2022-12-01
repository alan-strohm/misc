package day01

import (
	"bufio"
	"fmt"
	"sort"
)

type entry struct {
	blank bool
	cals  int
}

var blank = entry{blank: true}

func cals(n int) entry { return entry{cals: n} }

func parse(line string) (e entry, err error) {
	if len(line) == 0 {
		return blank, nil
	}
	var n int
	if _, err = fmt.Sscanf(line, "%d", &n); err != nil {
		return
	}
	return cals(n), nil
}

func Run(s *bufio.Scanner, part1 bool) (int, error) {
	cur := 0
	sums := []int{}
	for s.Scan() {
		e, err := parse(s.Text())
		if err != nil {
			return 0, err
		}
		if e.blank {
			sums = append(sums, cur)
			cur = 0
		} else {
			cur += e.cals
		}
	}
	sort.Ints(sums)
	if part1 {
		return sums[len(sums)-1], nil
	}
	sums = sums[len(sums)-3:]
	return sums[0] + sums[1] + sums[2], nil
}
