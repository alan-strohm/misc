package day08

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 26},
		{FName: "./input.txt", Part1: true, Want: 525},
		{FName: "./sample.txt", Part1: false, Want: 61229},
		{FName: "./input.txt", Part1: false, Want: 1083859},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
