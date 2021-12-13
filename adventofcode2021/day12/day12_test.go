package day12

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample1.txt", Part1: true, Want: 10},
		{FName: "./sample2.txt", Part1: true, Want: 19},
		{FName: "./sample3.txt", Part1: true, Want: 226},
		{FName: "./input.txt", Part1: true, Want: 4970},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
