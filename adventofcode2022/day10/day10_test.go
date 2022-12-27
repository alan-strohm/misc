package day10

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex.txt", Part1: true, Want: 13140},
		{FName: "./input.txt", Part1: true, Want: 12540},
	}
	lib.Test(t, testCases, Run)
}
