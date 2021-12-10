package day10

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 26397},
		{FName: "./input.txt", Part1: true, Want: 345441},
		{FName: "./sample.txt", Part1: false, Want: 288957},
		{FName: "./input.txt", Part1: false, Want: 3235371166},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
