package day07

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2022/lib"
)

func TestAll(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./ex.txt", Part1: true, Want: 95437},
		{FName: "./input.txt", Part1: true, Want: 2104783},
		{FName: "./ex.txt", Part1: false, Want: 24933642},
		{FName: "./input.txt", Part1: false, Want: 5883165},
	}
	lib.Test(t, testCases, Run)
}
