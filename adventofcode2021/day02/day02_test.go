package day02

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 1524750},
		{FName: "./input.txt", Part1: false, Want: 1592426537},
	}
	lib.Test(t, testCases, Run)
}
