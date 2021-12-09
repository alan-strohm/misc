package day08

import (
	"bufio"
	"fmt"
	"math"
	"sort"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type entry [4]string

type notes struct {
	es []entry
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &notes{}
	for scanner.Scan() {
		e := &entry{}
		_, err := fmt.Sscanln(scanner.Text(), e)
		if err != nil {
			return nil, err
		}
		r.es = append(r.es, *e)
	}
	return r, nil
}

func (e *entry) Scan(s fmt.ScanState, verb rune) error {
	outputs := [4]string{}
	signals := [10]string{}
	for i := 0; i < len(signals)+1+len(outputs); i++ {
		t, err := s.Token(true, nil)
		if err != nil {
			return err
		}

		iOutput := i - len(signals) - 1
		iSep := len(signals)

		switch {
		case i < len(signals):
			signals[i] = string(t)
		case i == iSep:
			if len(t) != 1 || t[0] != '|' {
				return fmt.Errorf("expected | got '%s'", string(t))
			}
			continue
		case iOutput >= 0:
			outputs[iOutput] = string(t)
		}
	}

	opts := newSegOpts()
	if err := opts.Analyze(signals); err != nil {
		return err
	}

	for i, from := range outputs {
		to := make([]byte, len(from))
		for j, b := range []byte(from) {
			to[j] = opts.lookup(b)
		}
		sort.Slice(to, func(i, j int) bool { return to[i] < to[j] })
		(*e)[i] = string(to)
	}
	return nil
}

type segSet map[byte]bool

func fromString(s string) segSet {
	out := make(map[byte]bool)
	for _, b := range []byte(s) {
		out[b] = true
	}
	return out
}

func allSegs() segSet {
	return fromString("abcdefg")
}

func (s segSet) String() string {
	r := make([]byte, 0, len(s))
	for k, _ := range s {
		r = append(r, k)
	}
	return string(r)
}

func (a segSet) intersect(b segSet) {
	for k, v := range a {
		if v != b[k] {
			delete(a, k)
		}
	}
}

func (a segSet) union(b segSet) {
	for k, _ := range b {
		a[k] = true
	}
}

func (a segSet) sub(b segSet) {
	for k, _ := range b {
		delete(a, k)
	}
}

var (
	canon = map[string]float64{
		"abcefg": 0, "cf": 1, "acdeg": 2, "acdfg": 3, "bcdf": 4,
		"abdfg": 5, "abdefg": 6, "acf": 7, "abcdefg": 8, "abcdfg": 9,
	}

	byLen      = map[int]segSet{}
	byByteFreq = map[int]segSet{}
)

func init() {
	byteFreqs := map[byte]int{}
	for s, _ := range canon {
		if byLen[len(s)] == nil {
			byLen[len(s)] = segSet{}
		}
		byLen[len(s)].union(fromString(s))
		for _, b := range []byte(s) {
			byteFreqs[b]++
		}
	}
	for k, v := range byteFreqs {
		if byByteFreq[v] == nil {
			byByteFreq[v] = segSet{}
		}
		byByteFreq[v][k] = true
	}
}

type segOpts map[byte]segSet

func newSegOpts() segOpts {
	r := segOpts{}
	for k, _ := range allSegs() {
		r[k] = allSegs()
	}
	return r
}

func (s segOpts) String() string {
	parts := make([]string, 0, len(s))
	for k, v := range s {
		parts = append(parts, fmt.Sprintf("%c: %s", k, &v))
	}
	return strings.Join(parts, ", ")
}

func (o segOpts) Analyze(signals [10]string) error {
	byteFreqs := map[byte]int{}
	for _, s := range signals {
		set, ok := byLen[len(s)]
		if !ok {
			return fmt.Errorf("invalid length signal: %s", s)
		}
		for _, b := range []byte(s) {
			o[b].intersect(set)
			byteFreqs[b]++
		}
	}
	for b, f := range byteFreqs {
		o[b].intersect(byByteFreq[f])
	}
	mapped := segSet{}
	for _, s := range o {
		if len(s) == 1 {
			mapped.union(s)
		}
	}
	for _, s := range o {
		if len(s) > 1 {
			s.sub(mapped)
		}
		if len(s) != 1 {
			return fmt.Errorf("unable to determine mapping from signals: %v (opts: %s)", signals, &o)
		}
	}
	return nil
}

func (o segOpts) lookup(seg byte) byte {
	return o[seg].String()[0]
}

func (n *notes) Part1() float64 {
	r := 0.0
	for _, e := range n.es {
		for _, o := range e {
			if len(o) == 7 || len(byLen[len(o)]) < 7 {
				r++
			}
		}
	}
	return r
}

func (n *notes) Part2() float64 {
	r := 0.0
	for _, e := range n.es {
		n := 0.0
		for i, o := range e {
			n += canon[o] * math.Pow(10, float64(len(e)-i-1))
		}
		lib.Dbg("%s: %f\n", strings.Join(e[:], " "), n)
		r += n
	}
	return r
}
