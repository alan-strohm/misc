package day05

import (
	"bufio"
	"fmt"
	"os"
	"unicode"
)

type stacks [][]rune

func must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func reverse(s []rune) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

func parseStacks(s *bufio.Scanner) *stacks {
	var r stacks
	for s.Scan() {
		runes := []rune(s.Text())
		if len(runes) >= 2 && unicode.IsDigit(runes[1]) {
			s.Scan()
			if len(s.Text()) > 0 {
				must(fmt.Errorf("expected empty line, got %s", s.Text()))
			}
			break
		}
		numStacks := (len(runes) + 1) / 4
		if r == nil {
			r = make([][]rune, numStacks)
		} else if len(r) != numStacks {
			must(fmt.Errorf("got line with %d stacks, want %d: \"%s\"", numStacks, len(r), s.Text()))
		}
		for i, _ := range r {
			crate := runes[i*4+1]
			if crate != ' ' && !unicode.IsUpper(crate) {
				must(fmt.Errorf("invalid crate %c in line %s", crate, s.Text()))
			}
			if crate != ' ' {
				r[i] = append(r[i], crate)
			}
		}
	}
	for _, stack := range r {
		reverse(stack)
	}
	return &r
}

func (sp *stacks) move(num, src, dst int) {
	s := *sp
	newLen := len(s[src]) - num
	toMove := s[src][newLen:len(s[src])]
	s[src] = s[src][0:newLen]
	reverse(toMove)
	s[dst] = append(s[dst], toMove...)
}

func (sp *stacks) top() (r []rune) {
	for _, s := range *sp {
		r = append(r, s[len(s)-1])
	}
	return
}

type move struct {
	num, src, dst int
}

func parseMove(s string) (r move) {
	_, err := fmt.Sscanf(s, "move %d from %d to %d", &r.num, &r.src, &r.dst)
	r.src--
	r.dst--
	must(err)
	return
}

func part1(scanner *bufio.Scanner) string {
	s := parseStacks(scanner)
	for scanner.Scan() {
		m := parseMove(scanner.Text())
		s.move(m.num, m.src, m.dst)
	}
	return string(s.top())
}
