package day01

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./input.txt", Part1: true, Want: 69206},
		{FName: "./input.txt", Part1: false, Want: 69206},
	}
	lib.Test(t, testCases, Run)
}
