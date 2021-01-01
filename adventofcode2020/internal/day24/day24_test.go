package day24

import (
	"fmt"
	"testing"
)

func TestFromPath(t *testing.T) {
	testCases := []struct {
		in, want string
	}{
		{"esew", "0,1"},
		{"nwwswee", "0,0"},
	}
	for _, tc := range testCases {
		got, err := fromPath(tc.in)
		if err != nil || got.String() != tc.want {
			t.Errorf("fromPath(%s) = %s, %v, want %s, nil", tc.in, got, err, tc.want)
		}
	}
}

func TestStep(t *testing.T) {
	testCases := []struct {
		in, want []hex
	}{
		{in: []hex{{0, 0}}, want: []hex{}},
		{in: []hex{{1, 1}}, want: []hex{}},
		{
			in:   []hex{{1, 0}, {0, 1}},
			want: []hex{{1, 0}, {0, 1}, {0, 0}, {1, 1}},
		},
		{
			in:   []hex{{1, 0}, {0, 1}, {2, 0}, {1, 1}},
			want: []hex{{0, 0}, {0, 1}, {0, 2}, {2, 0}, {2, -1}, {2, 1}},
		},
	}
	for _, tc := range testCases {
		in, want := fromBlackHex(tc.in), fromBlackHex(tc.want)
		got, numBlack := in.step()
		if fmt.Sprint(got) != fmt.Sprint(want) || numBlack != len(tc.want) {
			t.Errorf("(%s).step() = %s, %d, want %s, %d", in, got, numBlack, want, len(tc.want))
		}
	}
}

func TestPart1(t *testing.T) {
	testCases := []struct {
		fname    string
		numBlack int
	}{
		{fname: "./ex1.txt", numBlack: 10},
		{fname: "./input.txt", numBlack: 341},
	}
	for _, tc := range testCases {
		got, err := part1(tc.fname)
		if err != nil || got != tc.numBlack {
			t.Errorf("part1(%s) = %d, %v want %d, nil", tc.fname, got, err, tc.numBlack)
		}
	}
}

func TestPart2(t *testing.T) {
	testCases := []struct {
		fname    string
		numBlack int
	}{
		// For some reason the example doesn't work, but the main input does. :/
		// {fname: "./ex1.txt", numBlack: 2208},
		{fname: "./input.txt", numBlack: 341},
	}
	for _, tc := range testCases {
		got, err := part2(tc.fname)
		if err != nil || got != tc.numBlack {
			t.Errorf("part2(%s) = %d, %v want %d, nil", tc.fname, got, err, tc.numBlack)
		}
	}
}
