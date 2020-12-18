package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var dbg = flag.Bool("dbg", false, "Print debug info.")
var rounds = flag.Int("rounds", 2020, "Number of rounds to run.")

func init() {
	flag.Parse()
}

func input() ([]int, error) {
	scanner := bufio.NewScanner(os.Stdin)
	if !scanner.Scan() {
		return nil, errors.New("no input")
	}
	l := scanner.Text()
	nums := strings.Split(l, ",")
	r := make([]int, len(nums))
	for i, num := range nums {
		var err error
		r[i], err = strconv.Atoi(num)
		if err != nil {
			return nil, fmt.Errorf("bad input: %s", l)
		}
	}
	return r, nil
}

type game struct {
	mem   map[int]int
	round int
}

func newGame() *game {
	return &game{mem: make(map[int]int)}
}

func (g *game) speak(num int) int {
	g.round++
	prev, ok := g.mem[num]
	g.mem[num] = g.round
	if !ok {
		return 0
	}
	return g.round - prev
}

func part1(in []int, target int) {
	g := newGame()
	next := 0
	for i := 0; i < target-1; i++ {
		if i < len(in) {
			next = g.speak(in[i])
		} else {
			next = g.speak(next)
		}
	}
	fmt.Printf("Last spoken: %d\n", next)
}

func main() {
	in, err := input()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	part1(in, *rounds)
}
