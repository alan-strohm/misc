package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func read() ([]int, error) {
	r := make([]int, 0)
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	for i := 0; scanner.Scan(); i++ {
		i, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, fmt.Errorf("on line %d: %s", i, err)
		}
		r = append(r, i)
	}
	return r, nil
}

func part1(in []int) error {
	needed := make(map[int]bool)
	for _, i := range in {
		if needed[i] {
			fmt.Printf("%d * %d = %d\n", 2020-i, i, i*(2020-i))
			return nil
		}
		needed[2020-i] = true
	}
	return fmt.Errorf("no 2 digits sum to 2020")
}

func part2(in []int) error {
	sort.Ints(in)
	for i, first := range in {
		for j, second := range in[i+1:] {
			for _, third := range in[j+1:] {
				if first+second+third == 2020 {
					fmt.Printf("%d * %d * %d = %d\n", first, second, third, first*second*third)
					return nil
				}
				if first+second+third > 2020 {
					break
				}
			}
		}
	}
	return fmt.Errorf("no 3 digits sum to 2020")
}

func main() {
	in, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part1(in); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part2(in); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
