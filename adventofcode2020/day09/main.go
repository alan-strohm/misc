package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Processor interface {
	ProcessLine(l string) (bool, error)
	Close() error
}

func Process(p Processor) error {
	f, err := os.Open("./input.txt")
	if err != nil {
		return fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if done, err := p.ProcessLine(scanner.Text()); err != nil || done {
			return err
		}
	}
	return p.Close()
}

type buf struct {
	count      map[int]int
	order      []int
	invalidNum int
}

func newBuf() *buf {
	return &buf{count: make(map[int]int), order: make([]int, 0), invalidNum: -1}
}

func (b *buf) Close() error {
	return nil
}

func (b *buf) ProcessLine(l string) (bool, error) {
	n, err := strconv.Atoi(l)
	if err != nil {
		return false, err
	}
	if len(b.order) >= 25 {
		for _, k := range b.order {
			if n-k == k || b.count[n-k] == 0 {
				continue
			}
			b.count[b.order[0]]--
			b.order = b.order[1:25]
			break
		}
	}
	if len(b.order) < 25 {
		b.order = append(b.order, n)
		b.count[n]++
		return false, nil
	}
	b.invalidNum = n
	return true, nil
}

func part1() error {
	b := newBuf()
	if err := Process(b); err != nil {
		return err
	}
	fmt.Printf("part1 answer: %d\n", b.invalidNum)
	return nil
}

type partialSum struct {
	target int
	parts  []int
	curSum int
}

func newSum(target int) *partialSum {
	return &partialSum{target: target, curSum: 0, parts: make([]int, 0)}
}

func (s *partialSum) ProcessLine(l string) (bool, error) {
	n, err := strconv.Atoi(l)
	if err != nil {
		return false, err
	}
	s.curSum = s.curSum + n
	s.parts = append(s.parts, n)
	for s.curSum > s.target {
		s.curSum -= s.parts[0]
		s.parts = s.parts[1:len(s.parts)]
	}
	return s.curSum == s.target, nil
}

func (s *partialSum) Close() error {
	return nil
}

func part2() error {
	p := newSum(15690279)
	if err := Process(p); err != nil {
		return err
	}
	min, max := p.parts[0], p.parts[0]
	for _, n := range p.parts {
		if n < min {
			min = n
		}
		if n > max {
			max = n
		}
	}
	fmt.Printf("part2 answer: %d + %d = %d\n", min, max, min+max)
	return nil
}

func main() {
	if err := part1(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part2(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
