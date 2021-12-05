package day05

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPoints(t *testing.T) {
	testCases := []struct {
		in   line
		want []point
	}{{
		in:   line{start: point{1, 1}, end: point{3, 1}},
		want: []point{{1, 1}, {2, 1}, {3, 1}},
	}, {
		in:   line{start: point{1, 1}, end: point{3, 3}},
		want: []point{{1, 1}, {2, 2}, {3, 3}},
	}}
	for _, tc := range testCases {
		got := tc.in.points()
		if len(got) != len(tc.want) {
			t.Errorf("len(%v.points()) = %d, want %d", tc.in, len(got), len(tc.want))
		}
		for i, g := range got {
			if !g.eq(&tc.want[i]) {
				t.Errorf("%v.points() = %v, want %v", tc.in, got, tc.want)
				break
			}
		}
	}
}

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 5},
		{FName: "./input.txt", Part1: true, Want: 6564},
		{FName: "./sample.txt", Part1: false, Want: 12},
		{FName: "./input.txt", Part1: false, Want: 19172},
	}
	lib.Test(t, testCases, Run)
}
