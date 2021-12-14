package day14

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 1588},
		{FName: "./input.txt", Part1: true, Want: 2584},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
