package day22

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func dbg(in string) {
	if *dbgFlag {
		fmt.Println(in)
	}
}

type card struct {
	val        int
	next, prev *card
}

type deck struct {
	top, bottom *card

	l, player int
}

func (d *deck) score() (score int) {
	for c, i := d.bottom, 1; c != nil; c, i = c.next, i+1 {
		score += i * c.val
		dbg(fmt.Sprintf("%d = %d*%d", score, i, c.val))
	}
	return
}

func (d *deck) String() string {
	parts := make([]string, 0)
	for c := d.top; c != nil; c = c.prev {
		parts = append(parts, strconv.Itoa(c.val))
	}
	return strings.Join(parts, ",")
}

func (d *deck) peek() int {
	if d.top == nil {
		return -1
	}
	return d.top.val
}

func (d *deck) max() int {
	max := 0
	for c, i := d.bottom, 1; c != nil; c, i = c.next, i+1 {
		if c.val > max {
			max = c.val
		}
	}
	return max
}

func (d *deck) pop() int {
	r := d.peek()
	if d.top != nil {
		d.top = d.top.prev
		if d.top != nil {
			d.top.next = nil
		}
	}
	d.l--
	return r
}

func (d *deck) push(val int) {
	c := &card{val: val}
	d.l++
	if d.bottom == nil {
		d.bottom = c
		d.top = c
		return
	}
	c.next = d.bottom
	d.bottom.prev = c
	d.bottom = c
}

func (d *deck) copyFirst(n int) *deck {
	if n > d.l {
		panic(fmt.Sprintf("could not copy %d cards from deck of length %d", n, d.l))
	}
	r := &deck{player: d.player}
	for c, i := d.top, 0; c != nil && i < n; c = c.prev {
		r.push(c.val)
	}
	return r
}

func parseDeck(s *bufio.Scanner) (*deck, error) {
	r := &deck{}
	for s.Scan() && s.Text() != "" {
		val, err := strconv.Atoi(s.Text())
		if err != nil {
			return nil, fmt.Errorf("malformed deck line: %s", s.Text())
		}
		r.push(val)
	}
	return r, nil
}

func readDecks(fname string) (p1, p2 *deck, err error) {
	f, err := os.Open(fname)
	if err != nil {
		err = fmt.Errorf("opening %s: %s", fname, err)
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for i, d := range []**deck{&p1, &p2} {
		if !scanner.Scan() || !strings.HasPrefix(scanner.Text(), "Player ") {
			err = fmt.Errorf("expected player line")
			return
		}
		*d, err = parseDeck(scanner)
		if err != nil {
			return
		}
		(*d).player = i + 1
	}
	return
}

func playGame1(d1, d2 *deck) *deck {
	for c1, c2, r := d1.peek(), d2.peek(), 0; c1 != -1 && c2 != -1; c1, c2, r = d1.peek(), d2.peek(), r+1 {
		dbg(fmt.Sprintf("Round %d: %d vs %d", r+1, c1, c2))
		dbg(fmt.Sprintf("Deck 1: %s", d1))
		dbg(fmt.Sprintf("Deck 2: %s", d2))
		d1.pop()
		d2.pop()
		w, h, l := d1, c1, c2
		if l > h {
			w, h, l = d2, c2, c1
		}
		w.push(h)
		w.push(l)
	}
	w := d1
	if d1.top == nil {
		w = d2
	}
	return w
}

var numRounds = 0

func playGame2(d1, d2 *deck, d int, gamePath string) *deck {
	dbg(fmt.Sprintf("Starting game %s; d1: %d, d2: %d", gamePath, d1.l, d2.l))
	if d > 0 {
		if max1, max2 := d1.max(), d2.max(); max1 > max2 {
			dbg(fmt.Sprintf("player 1 has the bigger card %d vs %d so they win", max1, max2))
			return d1
		}
	}
	prev := make(map[string]bool)
	for c1, c2, r := d1.peek(), d2.peek(), 0; c1 != -1 && c2 != -1; c1, c2, r = d1.peek(), d2.peek(), r+1 {
		numRounds++
		if numRounds%1_000_000 == 0 {
			dbg(fmt.Sprintf("Round %d million", numRounds/1_000_000))
		}
		dbg(fmt.Sprintf("Round %d: %d vs %d", r+1, c1, c2))
		dbg(fmt.Sprintf("Deck 1: %s", d1))
		dbg(fmt.Sprintf("Deck 2: %s", d2))

		roundID := d1.String() + "|" + d2.String()
		if prev[roundID] {
			// cache[gameID] = d1.player
			return d1
		}
		prev[roundID] = true

		d1.pop()
		d2.pop()
		w, h, l := d1, c1, c2
		if c1 > d1.l || c2 > d2.l {
			if l > h {
				w, h, l = d2, c2, c1
			}
		} else {
			wsub := playGame2(d1.copyFirst(c1), d2.copyFirst(c2), d+1, fmt.Sprintf("%s/round%d/game%d", gamePath, r, d+1))
			if w.player != wsub.player {
				w, h, l = d2, c2, c1
			}
		}
		w.push(h)
		w.push(l)
	}
	w := d1
	if d1.top == nil {
		w = d2
	}
	// cache[gameID] = w.player
	dbg(fmt.Sprintf("Game %d over, player %d wins", d, w.player))
	return w
}

func part1(fname string) (score int, err error) {
	d1, d2, err := readDecks(fname)
	if err != nil {
		return
	}
	w := playGame1(d1, d2)
	score = w.score()
	return
}

func part2(fname string) (score int, err error) {
	d1, d2, err := readDecks(fname)
	if err != nil {
		return
	}
	w := playGame2(d1, d2, 0, "/")
	score = w.score()
	return
}
