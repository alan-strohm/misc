package day23

import (
	"flag"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"
)

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func dbg(in string) {
	if *dbgFlag {
		fmt.Println(in)
	}
}

type cup struct {
	id         int
	label      int
	prev, next *cup
}

func (c *cup) removeSelf() {
	c.prev.next = c.next
	c.next.prev = c.prev
	c.next, c.prev = nil, nil
}

func (c *cup) insertAfter(ocs ...*cup) {
	curr := c
	if curr == nil {
		curr = ocs[0]
		curr.next, curr.prev = curr, curr
		ocs = ocs[1:]
	}
	next := curr.next
	for _, o := range ocs {
		curr.next = o
		o.prev = curr
		curr = o
	}
	curr.next = next
	next.prev = curr
}

type cups struct {
	curr *cup
	byID map[int]*cup
	num  int
}

func newCups(in []int) *cups {
	r := &cups{num: len(in), byID: make(map[int]*cup)}
	ordered := make([]int, len(in))
	copy(ordered, in)
	sort.Ints(ordered)
	byLabel := make(map[int]*cup)
	for id, label := range ordered {
		r.byID[id] = &cup{id: id, label: label}
		byLabel[label] = r.byID[id]
	}

	var curr *cup
	for _, label := range in {
		c := byLabel[label]
		curr.insertAfter(c)
		curr = c
	}
	r.curr = byLabel[in[0]]
	return r
}

func (c *cups) String() string {
	labels := make([]string, c.num)
	mid := c.num / 2
	labels[mid] = fmt.Sprintf("(%d)", c.curr.label)
	for curr, i := c.curr.prev, mid-1; i >= 0; curr, i = curr.prev, i-1 {
		labels[i] = strconv.Itoa(curr.label)
	}
	for curr, i := c.curr.next, mid+1; i < len(labels); curr, i = curr.next, i+1 {
		labels[i] = strconv.Itoa(curr.label)
	}
	return strings.Join(labels, ",")
}

const handSize = 3

var (
	handns   int64
	findns   int64
	insertns int64
)

func (cs *cups) move() {
	start := time.Now()
	hand := make([]*cup, handSize)
	hand[0] = cs.curr.next
	for i := 1; i < handSize; i++ {
		hand[i] = hand[i-1].next
	}
	for _, c := range hand {
		c.removeSelf()
	}

	handns += time.Now().Sub(start).Nanoseconds()
	start = time.Now()

	dstID := (cs.curr.id - 1 + cs.num) % cs.num
	for cs.byID[dstID] == nil || cs.byID[dstID].next == nil {
		dstID = (dstID - 1 + cs.num) % cs.num
	}

	findns += time.Now().Sub(start).Nanoseconds()
	start = time.Now()

	cs.byID[dstID].insertAfter(hand...)
	cs.curr = cs.curr.next

	insertns += time.Now().Sub(start).Nanoseconds()
}

func part1(in []int, numMoves int) string {
	cs := newCups(in)
	for i := 0; i < numMoves; i++ {
		cs.move()
		dbg(fmt.Sprintf("after move %d: %s", i, cs))
	}
	r := make([]string, 0, cs.num)
	for curr := cs.byID[0].next; len(r) == 0 || curr != cs.byID[0]; curr = curr.next {
		r = append(r, strconv.Itoa(curr.label))
	}
	return strings.Join(r, "")
}

func newCupsLegacy(in []int, num int) *cups {
	n := make([]int, num)
	copy(n, in)
	for i := len(in); i < len(n); i++ {
		n[i] = i + 1
	}
	return newCups(n)
}

const timingFreq = 10_000

func part2(cs *cups, numMoves int) int {
	start := time.Now()
	for i := 0; i < numMoves; i++ {
		cs.move()
		if i%(timingFreq+1) == timingFreq {
			elapsed := time.Now().Sub(start)
			remaining := time.Duration(elapsed.Nanoseconds()*int64(numMoves)/int64(i) - elapsed.Nanoseconds())
			fmt.Printf("elapsed: %s, hand: %s, find: %s, insert: %s, remaining: %s\n", elapsed, time.Duration(handns), time.Duration(findns), time.Duration(insertns), remaining)
		}
		// dbg(fmt.Sprintf("after move %d: %s", i, cs))
	}
	return cs.byID[0].next.label * cs.byID[0].next.next.label
}
