package day13

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 17},
		{FName: "./input.txt", Part1: true, Want: 689},
		{FName: "./sample.txt", Part1: false, Want: 16},
		{FName: "./input.txt", Part1: false, Want: 91},
	}
	lib.Test(t, testCases, Run)
}
