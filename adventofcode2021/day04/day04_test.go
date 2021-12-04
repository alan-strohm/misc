package day04

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 4512},
		{FName: "./input.txt", Part1: true, Want: 22680},
		{FName: "./sample.txt", Part1: false, Want: 1924},
		{FName: "./input.txt", Part1: false, Want: 16168},
	}
	lib.Test(t, testCases, Run)
}
