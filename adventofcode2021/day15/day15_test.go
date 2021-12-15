package day15

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 40, Dbg: true},
		{FName: "./input.txt", Part1: true, Want: 508},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
