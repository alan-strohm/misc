package day24

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestSub(t *testing.T) {
	testCases := [][3]*range_{
		{{-7, 1}, {1, 2}, {-7, 0}},
		{{1, 9}, {1, 2}, {3, 9}},
	}
	for _, tc := range testCases {
		if got := tc[0].sub(tc[1]); *got != *tc[2] {
			t.Errorf("%v.sub(%v) = %v, want %v", tc[0], tc[1], got, tc[2])
		}
	}
}

func TestDay24(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 29991993698469},
		{FName: "./input.txt", Part1: false, Want: 14691271141118},
	}
	lib.Test(t, testCases, Run)
}
