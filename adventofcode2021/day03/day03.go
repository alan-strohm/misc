package day03

import (
	"bufio"
	"strconv"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type trie struct {
	size  int
	pos   int
	child [2]*trie
}

func newTrie(size int) *trie {
	return &trie{pos: size}
}

func (t *trie) insert(val uint) {
	t.size++
	if t.pos == 0 {
		return
	}
	on := (val >> (t.pos - 1)) & 1
	if t.child[on] == nil {
		t.child[on] = &trie{pos: t.pos - 1}
	}
	t.child[on].insert(val)
}

// choose can assume that both children are non-nil
func (t *trie) find(choose func(child [2]*trie) *trie) uint {
	if t.child[0] == nil && t.child[1] == nil {
		return 0
	}
	next := t.child[0]
	if next == nil {
		next = t.child[1]
	} else if t.child[1] != nil {
		next = choose(t.child)
	}
	r := next.find(choose)
	if next == t.child[1] {
		r += 1 << (t.pos - 1)
	}
	return r
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	var all *trie
	for scanner.Scan() {
		if all == nil {
			all = newTrie(len(scanner.Text()))
		}
		val, err := strconv.ParseInt(scanner.Text(), 2, 32)
		if err != nil {
			return 0, err
		}
		all.insert(uint(val))
	}
	if p1 {
		return part1(all)
	}
	return part2(all)
}

func part1(all *trie) (int, error) {
	gamma, epsilon := 0, 0
	nodes := []*trie{all}
	for i := all.pos; i > 0; i-- {
		next := make([]*trie, 0, 2*len(nodes))
		sum := 0
		for _, n := range nodes {
			if n.child[0] != nil {
				sum -= n.child[0].size
				next = append(next, n.child[0])
			}
			if n.child[1] != nil {
				sum += n.child[1].size
				next = append(next, n.child[1])
			}
		}
		nodes = next

		if sum > 0 { // 1 is most common
			gamma += 1 << (i - 1)
		} else { // 1 is least common
			epsilon += 1 << (i - 1)
		}
	}

	lib.Dbg("gamma: %b, epsilon: %b\n", gamma, epsilon)
	return gamma * epsilon, nil
}

func part2(all *trie) (int, error) {
	oo := int(all.find(func(child [2]*trie) *trie {
		if child[1].size >= child[0].size {
			return child[1]
		}
		return child[0]
	}))
	co2 := int(all.find(func(child [2]*trie) *trie {
		if child[0].size <= child[1].size {
			return child[0]
		}
		return child[1]
	}))
	lib.Dbg("oo: %b, co2: %b\n", oo, co2)
	return oo * co2, nil
}
