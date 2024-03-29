package lib

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"testing"
)

type SolutionFn func(scanner *bufio.Scanner, part1 bool) (int, error)

type TestCase struct {
	FName string
	Part1 bool
	Want  int
	Dbg   bool
}

func Test(t *testing.T, tcs []*TestCase, s SolutionFn) {
	for _, tc := range tcs {
		f, err := os.Open(tc.FName)
		if err != nil {
			t.Fatalf("couldn't open file %s: %s", tc.FName, err)
		}
		defer f.Close()

		got, err := s(bufio.NewScanner(f), tc.Part1)
		if err != nil || got != tc.Want {
			t.Errorf("run(%s, %t) = %v, %v, want %v, %v", tc.FName, tc.Part1, got, err, tc.Want, nil)
		}
	}
}

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func Must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func Dbg(format string, a ...interface{}) {
	if *dbgFlag {
		fmt.Printf(format, a...)
	}
}
