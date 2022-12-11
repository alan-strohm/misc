package day10

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type op int

const (
	noop op = 1
	addx op = 2
)

func must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func part1(s *bufio.Scanner, periods []int) int {
	signalSum := 0
	numCycles := map[string]int{"noop": 1, "addx": 2}
	sort.Ints(periods)
	cycle, reg := 1, 1
	for s.Scan() {
		fields := strings.Fields(s.Text())
		time := numCycles[fields[0]]
		if len(periods) > 0 && periods[0] < cycle+time {
			signalSum += periods[0] * reg
			periods = periods[1:]
		}
		cycle += time
		if len(fields) > 1 {
			inc, err := strconv.Atoi(fields[1])
			must(err)
			reg += inc
		}
	}
	return signalSum
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	if isPart1 {
		return part1(s, []int{20, 60, 100, 140, 180, 220}), nil
	}
	return 0, nil
}
