package day06

import "bufio"

type startFinder struct {
	size int
	buf  []byte
	freq map[byte]int
}

func newFinder(size int) *startFinder {
	return &startFinder{size: size, freq: map[byte]int{}}
}

func (f *startFinder) nextChar(c byte) bool {
	f.buf = append(f.buf, c)
	f.freq[c]++

	if len(f.buf) < f.size {
		return false
	}

	if len(f.buf) > f.size {
		old := f.buf[0]
		f.buf = f.buf[1:]
		f.freq[old]--
	}
	for _, c := range f.buf {
		if f.freq[c] > 1 {
			return false
		}
	}
	return true
}

func findStart(size int, stream []byte) int {
	f := newFinder(size)
	for i, c := range stream {
		if f.nextChar(c) {
			return i + 1
		}
	}
	return -1
}

func Run(s *bufio.Scanner, part1 bool) (int, error) {
	s.Scan()
	if part1 {
		return findStart(4, []byte(s.Text())), nil
	}
	return findStart(14, []byte(s.Text())), nil
}
