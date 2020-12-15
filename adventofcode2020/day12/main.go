package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"
	"strconv"
)

var in = flag.String("in", "./input.txt", "File to read.")
var dbg = flag.Bool("dbg", false, "Print debug info.")

func init() {
	flag.Parse()
}

type Processor interface {
	ProcessLine(l string) (done bool, err error)
	Close() error
}

func Process(p Processor) error {
	f, err := os.Open(*in)
	if err != nil {
		return fmt.Errorf("opening %s: %s", *in, err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if done, err := p.ProcessLine(scanner.Text()); err != nil || done {
			return err
		}
	}
	return p.Close()
}

type point struct {
	e, n int
}

func (p point) String() string {
	return fmt.Sprintf("(%d, %d)", p.e, p.n)
}

type rot struct {
	e, n point
}

type ferryOp rune

func (op ferryOp) String() string {
	return string(op)
}

const (
	NOp   ferryOp = 'N'
	SOp           = 'S'
	EOp           = 'E'
	WOp           = 'W'
	FOp           = 'F'
	LOp           = 'L'
	ROp           = 'R'
	UnkOp         = '!'
)

func (p *point) rot(r rot) {
	e := p.e*r.e.e + p.n*r.n.e
	n := p.e*r.e.n + p.n*r.n.n
	p.e, p.n = e, n
}

func (p *point) scale(s int) point {
	return point{p.e * s, p.n * s}
}

func (p *point) inc(r point) {
	p.e += r.e
	p.n += r.n
}

func (p *point) manhattanDist() int {
	return int(math.Abs(float64(p.e)) + math.Abs(float64(p.n)))
}

var dirs = map[ferryOp]point{
	NOp: point{e: 0, n: 1},
	SOp: point{e: 0, n: -1},
	EOp: point{e: 1, n: 0},
	WOp: point{e: -1, n: 0},
}

func isDir(op ferryOp) bool {
	_, ok := dirs[op]
	return ok
}

var turns = map[ferryOp]rot{
	LOp: rot{e: point{e: 0, n: 1}, n: point{e: -1, n: 0}},
	ROp: rot{e: point{e: 0, n: -1}, n: point{e: 1, n: 0}},
}

func isTurn(op ferryOp) bool {
	_, ok := turns[op]
	return ok
}

type ferry struct {
	pos point
	dir point
}

func newFerry() *ferry {
	return &ferry{pos: point{e: 0, n: 0}, dir: point{e: 1, n: 0}}
}

func (f *ferry) Close() error {
	return nil
}

func (f *ferry) String() string {
	return fmt.Sprintf("pos: %v, dir: %v", f.pos, f.dir)
}

func parseLine(l string) (ferryOp, int, error) {
	cmd := []rune(l)
	if len(cmd) < 2 {
		return UnkOp, 0, fmt.Errorf("invalid line: %s", l)
	}
	op := ferryOp(cmd[0])
	arg, err := strconv.Atoi(string(cmd[1:]))
	if err != nil {
		return UnkOp, 0, fmt.Errorf("invalid line: %s", l)
	}
	return op, arg, nil
}

func (f *ferry) ProcessLine(l string) (bool, error) {
	op, arg, err := parseLine(l)
	if err != nil {
		return false, err
	}
	if turn, ok := turns[op]; ok {
		if arg%90 != 0 {
			return false, fmt.Errorf("invalid turn: %d", arg)
		}
		for i := 0; i < arg/90; i++ {
			f.dir.rot(turn)
		}
		if *dbg {
			fmt.Printf("%s -> %s\n", l, f)
		}
		return false, nil
	}

	dir := f.dir
	if opDir, ok := dirs[op]; ok {
		dir = opDir
	} else if op != FOp {
		return false, fmt.Errorf("unknown op: %s", op)
	}
	f.pos.inc(dir.scale(arg))
	if *dbg {
		fmt.Printf("%s -> %s\n", l, f)
	}
	return false, nil
}

func part1() error {
	f := newFerry()
	if err := Process(f); err != nil {
		return err
	}
	fmt.Printf("Ferry at %s, dist: %d\n", f.pos, f.pos.manhattanDist())
	return nil
}

type ferry2 struct {
	pos    point
	course point // Ferry's course vector from the origin
}

func newFerry2() *ferry2 {
	return &ferry2{pos: point{e: 0, n: 0}, course: point{e: 10, n: 1}}
}

func (f *ferry2) Close() error {
	return nil
}

func (f *ferry2) String() string {
	return fmt.Sprintf("pos: %v, course: %v", f.pos, f.course)
}

func (f *ferry2) ProcessLine(l string) (bool, error) {
	op, arg, err := parseLine(l)
	if err != nil {
		return false, err
	}
	if turn, ok := turns[op]; ok {
		if arg%90 != 0 {
			return false, fmt.Errorf("invalid turn: %d", arg)
		}
		for i := 0; i < arg/90; i++ {
			f.course.rot(turn)
		}
		if *dbg {
			fmt.Printf("%s -> %s\n", l, f)
		}
		return false, nil
	}

	if opDir, ok := dirs[op]; ok {
		f.course.inc(opDir.scale(arg))
	} else if op == FOp {
		f.pos.inc(f.course.scale(arg))
	} else {
		return false, fmt.Errorf("unknown op: %s", op)
	}
	if *dbg {
		fmt.Printf("%s -> %s\n", l, f)
	}
	return false, nil
}

func part2() error {
	f := newFerry2()
	if err := Process(f); err != nil {
		return err
	}
	fmt.Printf("Ferry at %s, dist: %d\n", f.pos, f.pos.manhattanDist())
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
