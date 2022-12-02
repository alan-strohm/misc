package day03

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestPriority(t *testing.T) {
	testCases := []struct {
		item     rune
		priority int
	}{
		{item: 'a', priority: 1}, {item: 'z', priority: 26},
		{item: 'A', priority: 27}, {item: 'Z', priority: 52},
	}
	for _, tc := range testCases {
		priority := priorityIndex(tc.item) + 1
		if priority != tc.priority {
			t.Errorf("priorityIndex(%c) = %d, want %d", tc.item, priority, tc.priority)
		}
	}
}

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 8298},
		{FName: "./input.txt", Part1: false, Want: 2708},
	}
	lib.Test(t, testCases, Run)
}
