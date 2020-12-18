package day15

import "testing"

func TestPlayGame(t *testing.T) {
	testCases := []struct {
		in           []int
		rounds, want int
	}{
		{in: []int{0, 3, 6}, rounds: 2020, want: 436},
		{in: []int{1, 3, 2}, rounds: 2020, want: 1},
		{in: []int{2, 1, 3}, rounds: 2020, want: 10},
		{in: []int{1, 2, 3}, rounds: 2020, want: 27},
		{in: []int{2, 3, 1}, rounds: 2020, want: 78},
		{in: []int{3, 2, 1}, rounds: 2020, want: 438},
		{in: []int{3, 1, 2}, rounds: 2020, want: 1836},
		{in: []int{0, 3, 6}, rounds: 30000000, want: 175594},
		{in: []int{1, 3, 2}, rounds: 30000000, want: 2578},
		{in: []int{2, 1, 3}, rounds: 30000000, want: 3544142},
		{in: []int{1, 2, 3}, rounds: 30000000, want: 261214},
		{in: []int{2, 3, 1}, rounds: 30000000, want: 6895259},
		{in: []int{3, 2, 1}, rounds: 30000000, want: 18},
		{in: []int{3, 1, 2}, rounds: 30000000, want: 362},
	}
	for _, tc := range testCases {
		got := PlayGame(tc.in, tc.rounds)
		if got != tc.want {
			t.Errorf("PlayGame(%v, %d) = %d; want %d", tc.in, tc.rounds, got, tc.want)
		}
	}
}
