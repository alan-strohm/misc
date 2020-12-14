package main

import (
	"bufio"
	"fmt"
	"os"
)

type slope struct {
	right, down int
}

func testSlopes(slopes []slope) ([]int, error) {
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan() // Ignore the first line.
	numTrees := make([]int, len(slopes))
	pos := make([]int, len(slopes))
	for y := 1; scanner.Scan(); y++ {
		line := []rune(scanner.Text())
		for si, s := range slopes {
			if y%s.down == 0 {
				pos[si] = (pos[si] + s.right) % len(line)
				if line[pos[si]] == '#' {
					numTrees[si]++
				}
			}
		}
	}
	return numTrees, nil
}

func part1() error {
	numTrees, err := testSlopes([]slope{{down: 1, right: 3}})
	if err != nil {
		return err
	}
	fmt.Printf("%d trees on a slope of 1 down, 3 right\n", numTrees[0])
	return nil
}

func part2() error {
	numTrees, err := testSlopes([]slope{
		{down: 1, right: 1},
		{down: 1, right: 3},
		{down: 1, right: 5},
		{down: 1, right: 7},
		{down: 2, right: 1},
	})
	if err != nil {
		return err
	}
	fmt.Printf("%d * %d * %d * %d * %d = %d\n", numTrees[0], numTrees[1], numTrees[2], numTrees[3], numTrees[4], numTrees[0]*numTrees[1]*numTrees[2]*numTrees[3]*numTrees[4])
	return nil
}

func main() {
	if err := part1(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part2(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
