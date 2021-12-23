package day19

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestManDist(t *testing.T) {
	a, b, want := point{1105, -1205, 1229}, point{-92, -2380, -20}, 3621
	got := manDist(a, b)
	if want != got {
		t.Errorf("manDist(%d, %d) = %d, want %d", a, b, got, want)
	}
}

func TestDay22(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 79},
		{FName: "./input.txt", Part1: true, Want: 335},
		{FName: "./sample.txt", Part1: false, Want: 3621},
		{FName: "./input.txt", Part1: false, Want: 10864},
	}
	lib.Test(t, testCases, Run)
}
