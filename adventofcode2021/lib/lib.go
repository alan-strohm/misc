package lib

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"testing"
)

type SolutionFn func(scanner *bufio.Scanner, Part1 bool) (int, error)

type Solution interface {
	Part1() int
	Part2() int
}

type SolutionFactory func(scanner *bufio.Scanner) (Solution, error)

func Convert(f SolutionFactory) SolutionFn {
	return func(scanner *bufio.Scanner, Part1 bool) (int, error) {
		s, err := f(scanner)
		if err != nil {
			return 0, err
		}
		if Part1 {
			return s.Part1(), nil
		}
		return s.Part2(), nil
	}
}

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

		*dbgFlag = tc.Dbg
		got, err := s(bufio.NewScanner(f), tc.Part1)
		if err != nil || got != tc.Want {
			t.Errorf("run(%s, %t) = %v, %v, want %v, %v", tc.FName, tc.Part1, got, err, tc.Want, nil)
		}
	}
}

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func Dbg(format string, a ...interface{}) {
	if *dbgFlag {
		fmt.Printf(format, a...)
	}
}

var part1Flag = flag.Bool("part1", true, "Run part1 logic.")

func Run(fname string, s SolutionFn) {
	f, err := os.Open(fname)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	out, err := s(bufio.NewScanner(f), *part1Flag)
	if err != nil {
		panic(err)
	}
	fmt.Println(out)
}
