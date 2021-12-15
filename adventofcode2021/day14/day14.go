package day14

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type id [2]byte

type man struct {
	rules map[id][2]id
	tmpl  []id
}

func (m *man) charFreqs(steps int) map[byte]int {
	idFreqs := make(map[id]int)
	for _, id := range m.tmpl {
		idFreqs[id]++
	}
	for i := 0; i < steps; i++ {
		nextFreqs := make(map[id]int)
		for id, count := range idFreqs {
			for _, cid := range m.rules[id] {
				nextFreqs[cid] += count
			}
		}
		idFreqs = nextFreqs
	}

	r := make(map[byte]int)
	rightMost := m.tmpl[len(m.tmpl)-1][1]
	r[rightMost]++

	for i, count := range idFreqs {
		r[i[0]] += count
	}
	return r
}

func result(freqs map[byte]int) int {
	lib.Dbg("%v\n", freqs)
	var most, least int
	for _, freq := range freqs {
		if least == 0 {
			least = freq
		}
		if freq > most {
			most = freq
		}
		if freq < least {
			least = freq
		}
	}
	return most - least
}

func (m *man) Part1() int {
	return result(m.charFreqs(10))
}

func (m *man) Part2() int {
	return result(m.charFreqs(40))
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	m := &man{rules: make(map[id][2]id)}
	scanner.Scan()
	tmpl := scanner.Text()
	scanner.Scan() // Blank
	for scanner.Scan() {
		var l, r, to byte
		if _, err := fmt.Sscanf(scanner.Text(), "%c%c -> %c", &l, &r, &to); err != nil {
			return nil, err
		}
		m.rules[id{l, r}] = [2]id{{l, to}, {to, r}}
	}

	m.tmpl = make([]id, len(tmpl)-1)
	var prev byte
	for i, c := range []byte(tmpl) {
		if i != 0 {
			m.tmpl[i-1] = id{prev, c}
		}
		prev = c
	}
	return m, nil
}
