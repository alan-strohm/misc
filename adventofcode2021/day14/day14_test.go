package day14

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 1588},
		{FName: "./input.txt", Part1: true, Want: 2584},
		{FName: "./sample.txt", Part1: false, Want: 2188189693529},
		{FName: "./input.txt", Part1: false, Want: 3816397135460},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
