package day03

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 198},
		{FName: "./input.txt", Part1: true, Want: 3901196},
		{FName: "./sample.txt", Part1: false, Want: 230},
		{FName: "./input.txt", Part1: false, Want: 4412188},
	}
	lib.Test(t, testCases, Run)
}
