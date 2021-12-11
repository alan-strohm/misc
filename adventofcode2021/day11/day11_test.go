package day11

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestNeighbors(t *testing.T) {
	testCases := []struct {
		in   point
		want []point
	}{
		{in: point{0, 0}, want: []point{{0, 1}, {1, 0}, {1, 1}}},
		{in: point{9, 9}, want: []point{{8, 8}, {8, 9}, {9, 8}}},
	}
	for _, tc := range testCases {
		got := tc.in.neighbors()
		if fmt.Sprintf("%v", got) != fmt.Sprintf("%v", tc.want) {
			t.Errorf("%v.neighbors() = %v, want %v", tc.in, got, tc.want)
		}
	}
}

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 1656},
		{FName: "./input.txt", Part1: true, Want: 1681},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
