package day14

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type state struct {
	id          uint8
	l, r, to    byte
	left, right *state
}

type states struct {
	all []*state
	cur []uint8
}

func (s *states) next() {
	next := make([]uint8, 0, 2*len(s.cur))
	for _, id := range s.cur {
		next = append(next, s.all[id].left.id)
		next = append(next, s.all[id].right.id)
	}
	s.cur = next
}

func (s *states) String() string {
	r := make([]byte, len(s.cur)+1)
	for i, id := range s.cur {
		r[i] = s.all[id].l
		if i+1 == len(s.cur) {
			r[i+1] = s.all[id].r
		}
	}
	return string(r)
}

func (s *states) Part1() int {
	for i := 0; i < 10; i++ {
		s.next()
	}
	freqs := map[byte]int{}
	for i, id := range s.cur {
		freqs[s.all[id].l]++
		if i+1 == len(s.cur) {
			freqs[s.all[id].r]++
		}
	}
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

func (s *states) Part2() int { return 0 }

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &states{}
	scanner.Scan()
	seed := scanner.Text()
	scanner.Scan() // Blank
	ids := map[string]uint8{}
	for scanner.Scan() {
		st := state{id: uint8(len(r.all))}
		if _, err := fmt.Sscanf(scanner.Text(), "%c%c -> %c", &st.l, &st.r, &st.to); err != nil {
			return nil, err
		}
		ids[string([]byte{st.l, st.r})] = st.id
		r.all = append(r.all, &st)
	}
	for _, st := range r.all {
		st.left = r.all[ids[string([]byte{st.l, st.to})]]
		st.right = r.all[ids[string([]byte{st.to, st.r})]]
	}
	var prev byte
	for i, c := range []byte(seed) {
		if i != 0 {
			r.cur = append(r.cur, ids[string([]byte{prev, c})])
		}
		prev = c
	}
	return r, nil
}
