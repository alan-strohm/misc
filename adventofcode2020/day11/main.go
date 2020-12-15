package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
)

var stdIn = flag.Bool("stdin", false, "Read inputs from stdin.")

func init() {
	flag.Parse()
}

type Processor interface {
	ProcessLine(l string) (bool, error)
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

type cellState rune

const (
	floorCell    cellState = '.'
	occupiedCell           = '#'
	emptyCell              = 'L'
	unknownCell            = '!'
)

type area struct {
	grid        [][]cellState
	crowdedFunc func(a *area, p point) int
	maxCrowded  int
}

type point struct {
	c, r int
}

var dirs = []point{
	{0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1},
}

func (p point) add(n point) point {
	return point{p.c + n.c, p.r + n.r}
}

func (p point) neighbors() []point {
	r := make([]point, len(dirs))
	for i, dir := range dirs {
		r[i] = p.add(dir)
	}
	return r
}

func newArea() *area {
	return &area{
		grid:        make([][]cellState, 0),
		crowdedFunc: crowdedPart1,
		maxCrowded:  4,
	}
}

func (a *area) setPart2() {
	a.maxCrowded = 5
	a.crowdedFunc = crowdedPart2
}

func (a *area) Close() error {
	return nil
}

func (a *area) ProcessLine(l string) (bool, error) {
	runes := []rune(l)
	row := make([]cellState, len(runes))
	for i, r := range runes {
		s := cellState(r)
		if s != floorCell && s != emptyCell {
			return false, fmt.Errorf("invalid cell state: %s", string(r))
		}
		row[i] = s
	}
	a.grid = append(a.grid, row)
	return false, nil
}

func (a *area) checkBounds(p point) error {
	if p.r >= len(a.grid) || p.r < 0 {
		return fmt.Errorf("row oob: %d vs %d", p.r, len(a.grid))
	}
	if p.c >= len(a.grid[p.r]) || p.c < 0 {
		return fmt.Errorf("column oob: %d vs %d", p.r, len(a.grid[p.r]))
	}
	return nil
}

func (a *area) getState(p point) (cellState, error) {
	if err := a.checkBounds(p); err != nil {
		return unknownCell, err
	}
	return a.grid[p.r][p.c], nil
}

func crowdedPart2(a *area, p point) int {
	if err := a.checkBounds(p); err != nil {
		log.Fatal("Error: %s", err)
		return 0
	}
	sum := 0
	for _, dir := range dirs {
		var err error
		for n, ns := p.add(dir), floorCell; err == nil && ns == floorCell; n = n.add(dir) {
			ns, err = a.getState(n)
			if err == nil && ns == occupiedCell {
				sum++
			}
		}
	}
	return sum
}

func crowdedPart1(a *area, p point) int {
	if err := a.checkBounds(p); err != nil {
		log.Fatal("Error: %s", err)
		return 0
	}
	sum := 0
	for _, n := range p.neighbors() {
		if ns, err := a.getState(n); err == nil && ns == occupiedCell {
			sum++
		}
	}
	return sum
}

func (a *area) iterate1() int {
	numOccupied := 0
	nextGrid := make([][]cellState, len(a.grid))
	for r, row := range a.grid {
		nextGrid[r] = make([]cellState, len(row))
		copy(nextGrid[r], row)
		for c, s := range row {
			p := point{r: r, c: c}
			crowdedScore := a.crowdedFunc(a, p)
			if s == emptyCell && crowdedScore == 0 {
				nextGrid[r][c] = occupiedCell
			}
			if s == occupiedCell && crowdedScore >= a.maxCrowded {
				nextGrid[r][c] = emptyCell
			}
			if nextGrid[r][c] == occupiedCell {
				numOccupied++
			}
		}
	}
	a.grid = nextGrid
	return numOccupied
}

func read() (*area, error) {
	a := newArea()
	if err := Process(a); err != nil {
		return nil, err
	}
	return a, nil
}

func run(a *area) {
	curr := 0
	for next := a.iterate1(); curr != next; next = a.iterate1() {
		curr = next
	}
	fmt.Printf("Number occupied: %d\n", curr)
}

func main() {
	a, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	a.setPart2()
	run(a)
}
