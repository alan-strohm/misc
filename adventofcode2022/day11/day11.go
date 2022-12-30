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

type op func(int) int

func addOp(term int) op {
	return func(old int) int {
		return old + term
	}
}
func mulOp(factor int) op {
	return func(old int) int {
		return old * factor
	}
}

var squareOp op = func(old int) int {
	return old * old
}

type testParams struct {
	update  op
	divides int
	next    func(bool) int
}

type item func(p *testParams) int

type itemFac func(init int) item

func newRemItemFactory(mods []int) itemFac {
	return func(init int) item {
		rems := map[int]int{}
		for _, mod := range mods {
			rems[mod] = init % mod
		}
		return func(p *testParams) int {
			for mod, rem := range rems {
				rems[mod] = p.update(rem) % mod
			}
			return p.next(rems[p.divides] == 0)
		}
	}
}

var newDivItem itemFac = func(val int) item {
	return func(p *testParams) int {
		start := val
		val = p.update(val) / 3
		next := p.next(val%p.divides == 0)
		fmt.Printf("update %d to %d then %d, next monkey: %d\n", start, p.update(start), val, next)
		return next
	}
}

type monkey struct {
	id           int
	initItems    []int
	params       *testParams
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

func parseOp(s *bufio.Scanner) op {
	if s.Text() == "  Operation: new = old * old" {
		return squareOp
	}
	var op rune
	var operand int
	parseLine(s, "  Operation: new = old %c %d", &op, &operand)
	switch op {
	case '*':
		return mulOp(operand)
	case '+':
		return addOp(operand)
	default:
		lib.Must(fmt.Errorf("unknown operand: %c", op))
		return nil
	}
}

func parseMonkey(s *bufio.Scanner) *monkey {
	if s.Text() == "" {
		expectLine(s)
	}
	r := monkey{params: &testParams{}}
	parseLine(s, "Monkey %d:", &r.id)
	expectLine(s)
	r.initItems = parseItems(s)
	expectLine(s)
	r.params.update = parseOp(s)
	expectLine(s)
	parseLine(s, "  Test: divisible by %d", &r.params.divides)
	expectLine(s)
	var trueMonkey, falseMonkey int
	parseLine(s, "    If true: throw to monkey %d", &trueMonkey)
	expectLine(s)
	parseLine(s, "    If false: throw to monkey %d", &falseMonkey)
	r.params.next = func(result bool) int {
		if result {
			return trueMonkey
		}
		return falseMonkey
	}
	return &r
}

type simulation struct {
	byID    map[int]*monkey
	items   map[int][]item
	ids     []int
	monkeys []*monkey
}

func newSimulation(monkeys []*monkey, fac itemFac) *simulation {
	r := &simulation{
		byID:    map[int]*monkey{},
		items:   map[int][]item{},
		monkeys: monkeys,
		ids:     make([]int, len(monkeys)),
	}
	for i, m := range monkeys {
		r.byID[m.id] = m
		r.ids[i] = m.id
		for _, init := range m.initItems {
			r.items[m.id] = append(r.items[m.id], fac(init))
		}
	}
	sort.Ints(r.ids)
	return r
}

func (s *simulation) runRound() {
	for _, id := range s.ids {
		for m := s.byID[id]; len(s.items[id]) > 0; s.items[id] = s.items[id][1:] {
			item := s.items[id][0]
			nextID := item(m.params)
			s.items[nextID] = append(s.items[nextID], item)
			m.numInspected++
		}
	}
}

func (s *simulation) monkeyBusiness() int {
	slices.SortFunc(s.monkeys, func(a, b *monkey) bool {
		return a.numInspected > b.numInspected
	})
	return s.monkeys[0].numInspected * s.monkeys[1].numInspected
}

var debugRounds = map[int]bool{
	1: true, 20: true,
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	monkeys := []*monkey{}
	for s.Scan() {
		monkeys = append(monkeys, parseMonkey(s))
	}
	mods := make([]int, len(monkeys))
	for i, m := range monkeys {
		mods[i] = m.params.divides
	}
	numRounds := 20
	var fac itemFac = newDivItem
	if !isPart1 {
		numRounds = 10000
		fac = newRemItemFactory(mods)
	}
	sim := newSimulation(monkeys, fac)
	for i := 0; i < numRounds; i++ {
		sim.runRound()
		if !debugRounds[i+1] {
			continue
		}
		fmt.Printf("== After round %d ==\n", i+1)
		for _, id := range sim.ids {
			fmt.Printf("Monkey %d inspected items %d times\n", id, sim.monkeys[id].numInspected)
		}
	}
	return sim.monkeyBusiness(), nil
}
