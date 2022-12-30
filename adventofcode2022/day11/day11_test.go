package day11

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex.txt", Part1: true, Want: 10605},
		{FName: "./input.txt", Part1: true, Want: 54036},
		{FName: "./ex.txt", Part1: false, Want: 2713310158},
		{FName: "./input.txt", Part1: false, Want: 13237873355},
	}
	lib.Test(t, testCases, Run)
}
