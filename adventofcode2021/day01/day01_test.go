package day01

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 1233},
		{FName: "./ex.txt", Part1: true, Want: 7},
		{FName: "./ex.txt", Part1: false, Want: 5, Dbg: true},
		{FName: "./input.txt", Part1: false, Want: 1275},
	}
	lib.Test(t, testCases, Run)
}
