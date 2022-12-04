package day05

import (
	"bufio"
	"os"
	"testing"
)

func TestAll(t *testing.T) {
	testCases := []struct {
		part1 bool
		want  string
	}{
		{part1: true, want: "DHBJQJCCW"},
		{part1: false, want: "WJVRLSJJT"},
	}
	for _, tc := range testCases {
		fName := "./input.txt"
		f, err := os.Open(fName)
		if err != nil {
			t.Fatalf("couldn't open file %s: %s", fName, err)
		}
		defer f.Close()

		got := Run(bufio.NewScanner(f), tc.part1)
		if err != nil || got != tc.want {
			t.Errorf("Run(%s, %t) = %v, want %v", fName, tc.part1, got, tc.want)
		}
	}
}
