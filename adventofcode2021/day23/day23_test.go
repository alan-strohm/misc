package day23

import (
	"testing"
)

var sample = state{rooms: [4][]cell{
	{'B', 'A'}, {'C', 'D'}, {'B', 'C'}, {'D', 'A'}},
	size: 2}

var sample2 = state{rooms: [4][]cell{
	{'B', 'D', 'D', 'A'}, {'C', 'C', 'B', 'D'}, {'B', 'B', 'A', 'C'}, {'D', 'A', 'C', 'A'}},
	size: 4}

var input = state{rooms: [4][]cell{
	{'A', 'B'}, {'D', 'C'}, {'A', 'D'}, {'B', 'C'}},
	size: 2}

var input2 = state{rooms: [4][]cell{
	{'A', 'D', 'D', 'B'}, {'D', 'C', 'B', 'C'}, {'A', 'B', 'A', 'D'}, {'B', 'A', 'C', 'C'}},
	size: 4}

func TestDay23(t *testing.T) {
	testCases := []struct {
		in   state
		want int
	}{
		{in: sample, want: 12521},
		{in: input, want: 13455},
		{in: sample2, want: 44169},
		{in: input2, want: 43567},
	}
	for i, tc := range testCases {
		got := solve(&tc.in)
		if got != tc.want {
			t.Errorf("solve(%d)= %d, want %d", i, got, tc.want)
		}
	}
}
