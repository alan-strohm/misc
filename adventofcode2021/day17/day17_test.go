package day17

import (
	"testing"
)

const sample = "target area: x=20..30, y=-10..-5"
const input = "target area: x=143..177, y=-106..-71"

type testCase struct {
	in   string
	want int
}

func TestPart1(t *testing.T) {
	testCases := []testCase{
		{sample, 45},
		{input, 5565},
	}
	for _, tc := range testCases {
		if got := part1(tc.in); got != tc.want {
			t.Errorf("part1(%s) = %d, want %d", tc.in, got, tc.want)
		}
	}
}
