package day05

import (
	"bufio"
	"os"
	"testing"
)

func TestPart1(t *testing.T) {
	fName := "./input.txt"
	f, err := os.Open(fName)
	if err != nil {
		t.Fatalf("couldn't open file %s: %s", fName, err)
	}
	defer f.Close()

	got := part1(bufio.NewScanner(f))
	want := "DHBJQJCCW"
	if err != nil || got != want {
		t.Errorf("part1(%s) = %v, want %v", fName, got, want)
	}
}
