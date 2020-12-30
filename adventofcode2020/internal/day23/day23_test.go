package day23

import "testing"

func TestPart1(t *testing.T) {
	testCases := []struct {
		in       []int
		numMoves int
		want     string
	}{
		{
			in:       []int{3, 8, 9, 1, 2, 5, 4, 6, 7},
			numMoves: 10,
			want:     "92658374",
		},
		{
			in:       []int{1, 5, 7, 6, 2, 3, 9, 8, 4},
			numMoves: 100,
			want:     "58427369",
		},
	}
	for _, tc := range testCases {
		got := part1(tc.in, tc.numMoves)
		if got != tc.want {
			t.Errorf("part1(%v, %d) = %s, want %s", tc.in, tc.numMoves, got, tc.want)
		}
	}
}

func TestPart2(t *testing.T) {
	testCases := []struct {
		in            []int
		num, numMoves int
		want          int
	}{
		{
			in:  []int{3, 8, 9, 1, 2, 5, 4, 6, 7},
			num: 100, numMoves: 1_000,
			want: 5568,
		},
		{
			in:  []int{3, 8, 9, 1, 2, 5, 4, 6, 7},
			num: 1_000_000, numMoves: 10_000_000,
			want: 149245887792,
		},
		{
			in:  []int{1, 5, 7, 6, 2, 3, 9, 8, 4},
			num: 1_000_000, numMoves: 10_000_000,
			want: 149245887792,
		},
	}
	for _, tc := range testCases {
		cs := newCupsLegacy(tc.in, tc.num)
		got := part2(cs, tc.numMoves)
		if got != tc.want {
			t.Errorf("part1(%v, %d, %d) = %d, want %d", tc.in, tc.num, tc.numMoves, got, tc.want)
		}
	}
}
