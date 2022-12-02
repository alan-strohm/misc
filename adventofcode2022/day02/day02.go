package day02

import (
	"bufio"
	"fmt"
	"os"
)

// Rock = 0, Paper = 1, Scissors = 2,
type throw int

func parseThem(r rune) throw         { return throw(r - 'A') }
func parseUs(r rune) throw           { return throw(r - 'X') }
func (t throw) score() int           { return int(t) + 1 }
func (t throw) other(r result) throw { return throw((int(t) + int(r) + 3) % 3) }

// Loss = -1, Tie = 0, Win = 1
type result int

func play(us, them throw) result { return result((us-them+4)%3 - 1) }
func (r result) score() int      { return (int(r) + 1) * 3 }
func parseResult(r rune) result  { return result(r - 'Y') }

func scorePart1(themR, usR rune) int {
	them, us := parseThem(themR), parseUs(usR)
	r := play(us, them)
	return us.score() + r.score()
}

func scorePart2(themR, resultR rune) int {
	them, r := parseThem(themR), parseResult(resultR)
	us := them.other(r)
	return us.score() + r.score()
}

func must(err error) {
	if err != nil {
		fmt.Printf("got error: %s\n", err)
		os.Exit(1)
	}
}

func parse(l string) (them, us rune) {
	_, err := fmt.Sscanf(l, "%c %c", &them, &us)
	must(err)
	return
}

func Run(s *bufio.Scanner, part1 bool) (int, error) {
	sum := 0
	for s.Scan() {
		them, us := parse(s.Text())
		if part1 {
			sum += scorePart1(them, us)
		} else {
			sum += scorePart2(them, us)
		}
	}
	return sum, nil
}
