package day21

import "testing"

func TestPart1(t *testing.T) {
	testCases := []struct {
		start [2]int
		want  int
	}{
		{[2]int{4, 8}, 739785},
		{[2]int{6, 9}, 925605},
	}
	for _, tc := range testCases {
		got := part1(tc.start)
		if got != tc.want {
			t.Errorf("part1(%v) = %d, want %d", tc.start, got, tc.want)
		}
	}
}

func TestPart2(t *testing.T) {
	testCases := []struct {
		start [2]int
		want  int
	}{
		{[2]int{4, 8}, 444356092776315},
		{[2]int{6, 9}, 486638407378784},
	}
	for _, tc := range testCases {
		got := part2(tc.start)
		if got != tc.want {
			t.Errorf("part2(%v) = %d, want %d", tc.start, got, tc.want)
		}
	}
}
