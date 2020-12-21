package day19

import "testing"

func TestNumMatching(t *testing.T) {
	testCases := []struct {
		fname string
		want  int
	}{
		{fname: "./ex1.txt", want: 2},
		{fname: "./input.txt", want: 241},
	}
	for _, tc := range testCases {
		n, err := NumMatching(tc.fname)
		if err != nil {
			t.Errorf("failed: %s", err)
		}
		if n != tc.want {
			t.Errorf("got %d, want %d", n, tc.want)
		}
	}
}

func TestNumMatchingPart2(t *testing.T) {
	testCases := []struct {
		fname string
		want  int
	}{
		{fname: "./ex2.txt", want: 12},
		{fname: "./input.txt", want: 424},
	}
	for _, tc := range testCases {
		n, err := NumMatchingPart2(tc.fname)
		if err != nil {
			t.Errorf("failed: %s", err)
		}
		if n != tc.want {
			t.Errorf("got %d, want %d", n, tc.want)
		}
	}
}
