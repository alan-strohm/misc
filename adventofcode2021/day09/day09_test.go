package day09

import (
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestPart1(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 15},
		{FName: "./input.txt", Part1: true, Want: 550},
	}
	lib.Test(t, testCases, lib.Convert(New))
}
