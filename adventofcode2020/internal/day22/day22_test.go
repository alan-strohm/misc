package day22

import "testing"

func TestPart1(t *testing.T) {
	testCases := []struct {
		fname     string
		wantPart1 int
	}{
		{fname: "./ex1.txt", wantPart1: 306},
		// {fname: "./input.txt", wantPart1: 32495},
	}
	for _, tc := range testCases {
		dbg(tc.fname)
		part1, err := part1(tc.fname)
		if err != nil {
			t.Errorf("part1(%s) failed: %s", tc.fname, err)
		}
		if part1 != tc.wantPart1 {
			t.Errorf("part1(%s) = %d, want: %d", tc.fname, part1, tc.wantPart1)
		}
	}
}

func TestPart2(t *testing.T) {
	testCases := []struct {
		fname     string
		wantPart2 int
	}{
		{fname: "./ex1.txt", wantPart2: 291},
		{fname: "./inf.txt", wantPart2: 105},
		{fname: "./input.txt", wantPart2: 0},
	}
	for _, tc := range testCases {
		dbg(tc.fname)
		part2, err := part2(tc.fname)
		if err != nil {
			t.Errorf("part2(%s) failed: %s", tc.fname, err)
		}
		if part2 != tc.wantPart2 {
			t.Errorf("part2(%s) = %d, want: %d", tc.fname, part2, tc.wantPart2)
		}
	}
}
