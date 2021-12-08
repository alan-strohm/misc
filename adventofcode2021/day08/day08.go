package day08

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type pat []byte

type entry struct {
	signals  [10]pat
	outputs  [4]pat
	sigOpts  map[string][]int
	outOpts  map[string][]int
	sigToOut map[string]string
}

var lenOpts = map[int][]int{
	2: []int{1},
	3: []int{7},
	4: []int{4},
	5: []int{2, 3, 5},
	6: []int{0, 6, 9},
	7: []int{8},
}

func (e *entry) Scan(s fmt.ScanState, verb rune) error {
	e.sigOpts = make(map[string][]int)
	e.outOpts = make(map[string][]int)
	lib.Dbg(fmt.Sprintf("new entry\n"))
	for i := 0; i < len(e.signals)+len(e.outputs)+1; i++ {
		t, err := s.Token(true, nil)
		if err != nil {
			return err
		}
		iOutput := i - len(e.signals) - 1
		switch {
		case i < len(e.signals):
			e.signals[i] = t
			opts, ok := lenOpts[len(t)]
			if !ok {
				return fmt.Errorf("unexpected length for pattern: '%s'", string(t))
			}
			e.sigOpts[string(t)] = opts
		case i == len(e.signals):
			if len(t) != 1 || t[0] != '|' {
				return fmt.Errorf("expected | got '%s'", string(t))
			}
		case iOutput >= 0:
			e.outputs[iOutput] = t
			opts, ok := lenOpts[len(t)]
			if !ok {
				return fmt.Errorf("unexpected length for pattern: '%s'", string(t))
			}
			lib.Dbg("  out: '%s' opts: %#v\n", string(t), opts)
			e.outOpts[string(t)] = opts
		}
	}
	return nil
}

type notes struct {
	es []*entry
}

func (n *notes) Part1() float64 {
	r := 0.0
	for _, e := range n.es {
		for _, p := range e.outputs {
			if len(lenOpts[len(p)]) == 1 {
				r++
			}
		}
	}
	return r
}
func (n *notes) Part2() float64 { return 0.0 }

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &notes{}
	for scanner.Scan() {
		e := &entry{}
		_, err := fmt.Sscanln(scanner.Text(), e)
		if err != nil {
			return nil, err
		}
		r.es = append(r.es, e)
	}
	return r, nil
}
