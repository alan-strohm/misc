package day07

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 37},
		{FName: "./input.txt", Part1: true, Want: 355521},
	}
	lib.Test(t, testCases, Run)
}