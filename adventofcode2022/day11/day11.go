package day11

import (
	"bufio"
	"errors"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/exp/slices"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

type monkey struct {
	id           int
	items        []int
	op           func(int) int
	test         int
	trueMonkey   int
	falseMonkey  int
	numInspected int
}

func expectLine(s *bufio.Scanner) {
	if !s.Scan() {
		lib.Must(errors.New("unexpected eof"))
	}
}

func parseLine(s *bufio.Scanner, format string, a ...any) {
	_, err := fmt.Sscanf(s.Text(), format, a...)
	if err != nil {
		lib.Must(fmt.Errorf("failed to parse line [%s] using format [%s]: %s", s.Text(), format, err))
	}
}

func parseItems(s *bufio.Scanner) []int {
	items := strings.Split(s.Text(), ", ")
	items[0] = strings.TrimPrefix(items[0], "  Starting items: ")
	r := make([]int, len(items))
	for i, item := range items {
		var err error
		r[i], err = strconv.Atoi(item)
		lib.Must(err)
	}
	return r
}

func parseOp(s *bufio.Scanner) func(int) int {
	if s.Text() == "  Operation: new = old * old" {
		return func(old int) int {
			return old * old
		}
	}
	var op rune
	var operand int
	parseLine(s, "  Operation: new = old %c %d", &op, &operand)
	switch op {
	case '*':
		return func(old int) int {
			return old * operand
		}
	case '+':
		return func(old int) int {
			return old + operand
		}
	default:
		lib.Must(fmt.Errorf("unknown operand: %c", op))
		return nil
	}
}

func parseMonkey(s *bufio.Scanner) *monkey {
	if s.Text() == "" {
		expectLine(s)
	}
	r := monkey{}
	parseLine(s, "Monkey %d:", &r.id)
	expectLine(s)
	r.items = parseItems(s)
	expectLine(s)
	r.op = parseOp(s)
	expectLine(s)
	parseLine(s, "  Test: divisible by %d", &r.test)
	expectLine(s)
	parseLine(s, "    If true: throw to monkey %d", &r.trueMonkey)
	expectLine(s)
	parseLine(s, "    If false: throw to monkey %d", &r.falseMonkey)
	return &r
}

type simulation struct {
	byID    map[int]*monkey
	ids     []int
	monkeys []*monkey
}

func newSimulation(monkeys []*monkey) *simulation {
	r := &simulation{byID: map[int]*monkey{}, monkeys: monkeys}
	for _, m := range monkeys {
		r.byID[m.id] = m
		r.ids = append(r.ids, m.id)
	}
	sort.Ints(r.ids)
	return r
}

func (s *simulation) runRound() {
	for _, id := range s.ids {
		for m := s.byID[id]; len(m.items) > 0; m.items = m.items[1:] {
			item := m.items[0]
			m.numInspected++
			item = m.op(item)
			item /= 3
			nextID := m.falseMonkey
			if item%m.test == 0 {
				nextID = m.trueMonkey
			}
			next := s.byID[nextID]
			next.items = append(next.items, item)
		}
	}
}

func (s *simulation) monkeyBusiness() int {
	slices.SortFunc(s.monkeys, func(a, b *monkey) bool {
		return a.numInspected > b.numInspected
	})
	return s.monkeys[0].numInspected * s.monkeys[1].numInspected
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	monkeys := []*monkey{}
	for s.Scan() {
		monkeys = append(monkeys, parseMonkey(s))
	}
	sim := newSimulation(monkeys)
	for i := 0; i < 20; i++ {
		sim.runRound()
	}
	if isPart1 {
		return sim.monkeyBusiness(), nil
	}
	return 0, nil
}
