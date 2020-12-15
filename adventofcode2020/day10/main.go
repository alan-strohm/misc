package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strconv"
)

var stdIn = flag.Bool("stdin", false, "Read inputs from stdin.")

func init() {
	flag.Parse()
}

type Processor interface {
	ProcessLine(l string) (done bool, err error)
	Close() error
}

func Process(p Processor) error {
	f, err := os.Open("./input.txt")
	if err != nil {
		return fmt.Errorf("opening input.txt: %s", err)
	}
	defer f.Close()

	if *stdIn {
		f = os.Stdin
	}

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if done, err := p.ProcessLine(scanner.Text()); err != nil || done {
			return err
		}
	}
	return p.Close()
}

type jolts []int

func (j *jolts) Close() error {
	return nil
}

func (j *jolts) ProcessLine(l string) (bool, error) {
	n, err := strconv.Atoi(l)
	if err != nil {
		return false, err
	}
	*j = append(*j, n)
	return false, nil
}

func read() (*jolts, error) {
	j := &jolts{}
	if err := Process(j); err != nil {
		return nil, err
	}
	sort.Ints(*j)
	return j, nil
}

func run() error {
	j, err := read()
	if err != nil {
		return err
	}
	deltaStats := make([]int, 3)
	deltaStats[2] = 1
	runs := make([]int, 0)
	currRun := 0
	prev := 0
	for _, v := range *j {
		if v-prev > 3 {
			return fmt.Errorf("big diff between %d and %d", v, prev)
		}
		if v-prev == 1 {
			currRun++
		} else {
			if currRun > 1 {
				runs = append(runs, currRun)
			}
			currRun = 0
		}
		deltaStats[v-prev-1]++
		prev = v
	}
	if currRun > 1 {
		runs = append(runs, currRun)
	}
	fmt.Printf("deltaStats: %v\n", deltaStats)
	fmt.Printf("runs: %v\n", runs)
	numCombos := 1
	for _, length := range runs {
		switch length {
		case 2:
			numCombos *= 2
		case 3:
			numCombos *= 1 + 2 + 1
		case 4:
			numCombos *= 1 + 3 + 3
		default:
			return fmt.Errorf("Unhandled runlength: %d", length)
		}
	}
	fmt.Printf("numCombos: %d\n\n", numCombos)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
