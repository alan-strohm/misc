package day16

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestReadBits(t *testing.T) {
	testCases := []struct {
		in          []byte
		start, read uint8
		want        uint64
	}{
		{in: []byte{0xaa}, start: 0, read: 3, want: 5},
		{in: []byte{0xaa}, start: 5, read: 3, want: 2},
		{in: []byte{0xa1, 0xa0}, start: 7, read: 2, want: 3},
		{in: []byte{0xa1, 0xff, 0x00}, start: 7, read: 10, want: 0x3fe},
	}
	for _, tc := range testCases {
		d := &dec{buf: tc.in, bit: tc.start}
		got := d.readBits(tc.read)
		if got != tc.want {
			t.Errorf("(%b, %d).readBits(%d) = %d, want %d", tc.in, tc.start, tc.read, got, tc.want)
		}
	}
}

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex1.txt", Part1: true, Want: 6},
		{FName: "./ex2.txt", Part1: true, Want: 16},
		{FName: "./ex3.txt", Part1: true, Want: 12},
		{FName: "./ex4.txt", Part1: true, Want: 23},
		{FName: "./ex5.txt", Part1: true, Want: 31},
		{FName: "./input.txt", Part1: true, Want: 971},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
