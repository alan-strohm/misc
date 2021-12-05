package day05

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 5},
		{FName: "./input.txt", Part1: true, Want: 6564},
	}
	lib.Test(t, testCases, Run)
}
