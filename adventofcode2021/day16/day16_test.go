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
		wantBit     uint8
	}{
		{in: []byte{0xaa}, start: 0, read: 3, want: 5, wantBit: 3},
		{in: []byte{0xaa}, start: 5, read: 3, want: 2, wantBit: 0},
		{in: []byte{0xa1, 0xa0}, start: 7, read: 2, want: 3, wantBit: 1},
		{in: []byte{0xa1, 0xff, 0x00}, start: 7, read: 10, want: 0x3fe, wantBit: 1},
	}
	for _, tc := range testCases {
		d := &dec{buf: tc.in, bit: tc.start}
		got := d.readBits(tc.read)
		if got != tc.want || d.bit != tc.wantBit {
			t.Errorf("(%b, %d).readBits(%d) = %d, bit %d, want %d, bit %d", tc.in, tc.start, tc.read, got, d.bit, tc.want, tc.wantBit)
		}
	}
}

func TestEval(t *testing.T) {
	testCases := []struct {
		in   string
		want uint64
	}{
		{"C200B40A82", 3},
		{"04005AC33890", 54},
		{"880086C3E88112", 7},
		{"CE00C43D881120", 9},
		{"D8005AC2A8F0", 1},
		{"F600BC2D8F", 0},
		{"9C005AC2F8F0", 0},
		{"9C0141080250320F1802104A08", 1},
	}
	for _, tc := range testCases {
		d := &dec{}
		if got := d.decodeString(tc.in).eval(); got != tc.want {
			t.Errorf("%s.eval() = %d want %d", tc.in, got, tc.want)
		}
	}
}

func TestDay16(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex1.txt", Part1: true, Want: 6},
		{FName: "./ex2.txt", Part1: true, Want: 16},
		{FName: "./ex3.txt", Part1: true, Want: 12},
		{FName: "./ex4.txt", Part1: true, Want: 23},
		{FName: "./ex5.txt", Part1: true, Want: 31},
		{FName: "./input.txt", Part1: true, Want: 971},
		{FName: "./input.txt", Part1: false, Want: 831996589851},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
