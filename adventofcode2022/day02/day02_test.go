package day02

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 14069},
		{FName: "./input.txt", Part1: false, Want: 12411},
	}
	lib.Test(t, testCases, Run)
}
