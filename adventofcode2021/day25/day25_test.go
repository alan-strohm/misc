package day25

import (
	"bufio"
	"fmt"
	"os"
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestSteps(t *testing.T) {
	f, err := os.Open("./sample.txt")
	if err != nil {
		t.Fatalf("couldn't open file: %s", err)
	}
	defer f.Close()

	s := parse(bufio.NewScanner(f))
	for i, exp := range expSteps {
		s.run(1)
		if fmt.Sprint(s) != exp {
			t.Errorf("after step %d, got\n[%v], want\n[%s]", i+1, s, exp)
		}
	}
}

func TestDay25(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 58},
		{FName: "./input.txt", Part1: true, Want: 523},
	}
	lib.Test(t, testCases, Run)
}

var expSteps = []string{
	`....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v
`,
	`>.v.v>>..v
v.v.>>vv..
>v>.>.>.v.
>>v>v.>v>.
.>..v....v
.>v>>.v.v.
v....v>v>.
.vv..>>v..
v>.....vv.
`,
	`v>v.v>.>v.
v...>>.v.v
>vv>.>v>..
>>v>v.>.v>
..>....v..
.>.>v>v..v
..v..v>vv>
v.v..>>v..
.v>....v..
`,
}
