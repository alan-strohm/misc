package day08

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex.txt", Part1: true, Want: 21},
		// {FName: "./input.txt", Part1: true, Want: 1669},
		// {FName: "./input.txt", Part1: false, Want: 0},
	}
	lib.Test(t, testCases, Run)
}
