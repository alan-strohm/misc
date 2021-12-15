package day15

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 40},
		{FName: "./input.txt", Part1: true, Want: 508},
		{FName: "./sample.txt", Part1: false, Want: 315},
		{FName: "./input.txt", Part1: false, Want: 2872},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
