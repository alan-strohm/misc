package day10

import (
	"bufio"
	"fmt"
	"os"
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

func run(s *bufio.Scanner) int {
	periods := []int{20, 60, 100, 140, 180, 220}
	signalSum, reg := 0, 1
	toAdd := []int{}
	for i := 0; i < 240; i++ {
		if i%40 == 0 {
			fmt.Println()
		}
		if i%40 >= reg-1 && i%40 <= reg+1 {
			fmt.Printf("#")
		} else {
			fmt.Printf(".")
		}
		cycle := i + 1
		if len(periods) > 0 && periods[0] == cycle {
			signalSum += periods[0] * reg
			periods = periods[1:]
		}
		if len(toAdd) > 0 {
			reg += toAdd[0]
			toAdd = toAdd[1:]
			continue
		}
		if !s.Scan() {
			break
		}
		fields := strings.Fields(s.Text())
		if fields[0] == "addx" {
			inc, err := strconv.Atoi(fields[1])
			must(err)
			toAdd = append(toAdd, inc)
		}
	}
	fmt.Println()
	return signalSum
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	return run(s), nil
}
