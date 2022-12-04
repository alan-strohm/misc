package day04

import (
	"bufio"
	"fmt"
	"os"
)

type interval struct {
	lo, hi int
}

func (i interval) contains(o interval) bool {
	return i.lo <= o.lo && i.hi >= o.hi
}
func (i interval) overlaps(o interval) bool {
	if i.contains(o) || o.contains(i) {
		return true
	}
	return (i.lo <= o.lo && i.hi >= o.lo) || (i.hi >= o.hi && i.lo <= o.hi)
}

func must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func parse(line string) (interval, interval) {
	var lo1, hi1, lo2, hi2 int
	_, err := fmt.Sscanf(line, "%d-%d,%d-%d", &lo1, &hi1, &lo2, &hi2)
	must(err)
	return interval{lo: lo1, hi: hi1}, interval{lo: lo2, hi: hi2}
}

func Run(s *bufio.Scanner, part1 bool) (int, error) {
	part1r, part2r := 0, 0
	for s.Scan() {
		i1, i2 := parse(s.Text())
		if i1.contains(i2) || i2.contains(i1) {
			part1r++
		}
		if i1.overlaps(i2) {
			part2r++
		}
	}
	if part1 {
		return part1r, nil
	}
	return part2r, nil
}
