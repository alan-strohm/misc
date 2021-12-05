package day06

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 5934},
		{FName: "./input.txt", Part1: true, Want: 388419},
		{FName: "./sample.txt", Part1: false, Want: 26984457539},
		{FName: "./input.txt", Part1: false, Want: 1740449478328},
	}
	lib.Test(t, testCases, Run)
}
