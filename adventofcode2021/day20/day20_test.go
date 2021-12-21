package day20

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestDay20(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 35},
		{FName: "./input.txt", Part1: true, Want: 5391},
		{FName: "./sample.txt", Part1: false, Want: 3351},
		{FName: "./input.txt", Part1: false, Want: 16383},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
