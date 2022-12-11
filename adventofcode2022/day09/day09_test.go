package day09

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 6486},
		{FName: "./input.txt", Part1: false, Want: 2678},
	}
	lib.Test(t, testCases, Run)
}

func TestNewTail(t *testing.T) {
	testCases := []struct {
		h, t, want pos
	}{
		{h: [2]int{0, 0}, t: [2]int{0, 0}, want: [2]int{0, 0}},
		{h: [2]int{1, 0}, t: [2]int{0, 0}, want: [2]int{0, 0}},
		{h: [2]int{1, 1}, t: [2]int{0, 0}, want: [2]int{0, 0}},
		{h: [2]int{2, 0}, t: [2]int{0, 0}, want: [2]int{1, 0}},
		{h: [2]int{2, 1}, t: [2]int{0, 0}, want: [2]int{1, 1}},
		{h: [2]int{-2, -1}, t: [2]int{0, 0}, want: [2]int{-1, -1}},
		{h: [2]int{2, 2}, t: [2]int{2, 4}, want: [2]int{2, 3}},
	}
	for _, tc := range testCases {
		got := newTail(tc.h, tc.t)
		if fmt.Sprint(got) != fmt.Sprint(tc.want) {
			t.Errorf("newTail(%v, %v) = %v, want %v", tc.h, tc.t, got, tc.want)
		}
	}
}
