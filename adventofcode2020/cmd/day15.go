package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"

	"../internal/day15"
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

func main() {
	in, err := input()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("Last spoken: %d\n", day15.PlayGame(in, *rounds))
}
