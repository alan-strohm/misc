package day17

import (
	"fmt"
	"testing"
)

func numDistinct(points []point) int {
	set := make(map[string]bool)
	for _, p := range points {
		str := fmt.Sprintf("%v", p)
		set[str] = true
	}
	return len(set)
}

func TestNeighbors(t *testing.T) {
	testCases := []struct {
		points   []point
		wantLen  int
		wantDist int
	}{
		{points: originNeighbors(3), wantLen: 26, wantDist: 26},
		{points: originNeighbors(4), wantLen: 80, wantDist: 80},
	}
	for _, tc := range testCases {
		l := len(tc.points)
		if l != tc.wantLen {
			t.Errorf("got %d len, want %d", l, tc.wantLen)
		}
		dist := numDistinct(tc.points)
		if dist != tc.wantDist {
			t.Errorf("got %d distinct, want %d", dist, tc.wantDist)
		}
	}
}

func TestNumActiveCubes(t *testing.T) {
	testCases := []struct {
		fname      string
		boots, dim int
		numActive  int
	}{
		{fname: "./ex1.txt", boots: 1, dim: 3, numActive: 11},
		{fname: "./ex1.txt", boots: 6, dim: 3, numActive: 112},
		{fname: "./input.txt", boots: 6, dim: 3, numActive: 298},
		{fname: "./ex1.txt", boots: 1, dim: 4, numActive: 29},
		{fname: "./ex1.txt", boots: 6, dim: 4, numActive: 848},
		{fname: "./input.txt", boots: 6, dim: 4, numActive: 1792},
	}
	for _, tc := range testCases {
		num, err := NumActiveCubes(tc.fname, tc.dim, tc.boots)
		if err != nil {
			t.Errorf("want nil, got %s", err)
			continue
		}
		if num != tc.numActive {
			t.Errorf("want %d, got %d", tc.numActive, num)
		}
	}
}
