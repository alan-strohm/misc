package day18

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestParse(t *testing.T) {
	testCases := []struct {
		in   string
		want number
	}{
		{"[1,2]", number{
			cells: [16]cell{{1, true}, {}, {}, {}, {}, {}, {}, {}, {2, true}},
		}},
		{"[[1,2],3]", number{
			cells: [16]cell{{1, true}, {}, {}, {}, {2, true}, {}, {}, {}, {3, true}},
		}},
		{"[1,[2,3]", number{
			cells: [16]cell{{1, true}, {}, {}, {}, {}, {}, {}, {}, {2, true}, {}, {}, {}, {3, true}},
		}},
		{"[[1,9],[8,5]]", number{
			cells: [16]cell{{1, true}, {}, {}, {}, {9, true}, {}, {}, {}, {8, true}, {}, {}, {}, {5, true}},
		}},
		{"[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]", number{
			cells: [16]cell{
				{1, true}, {3, true}, {5, true}, {3, true}, {1, true}, {3, true}, {8, true}, {7, true},
				{4, true}, {9, true}, {6, true}, {9, true}, {8, true}, {2, true}, {7, true}, {3, true}},
		}},
	}
	for _, tc := range testCases {
		got := parse(tc.in)
		if fmt.Sprintf("%v", *got) != fmt.Sprintf("%v", tc.want) {
			t.Errorf("parse(%s) = %v, want %v", tc.in, *got, tc.want)
		}
	}
}

func TestPrint(t *testing.T) {
	testCases := []struct{ in, want string }{
		{"[1,2]", "[1,2]"},
		{"[[1,2],3]", "[[1,2],3]"},
		{"[[1,9],[8,5]]", "[[1,9],[8,5]]"},
		{"[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]", "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"},
	}
	for _, tc := range testCases {
		got := parse(tc.in)
		if got.String() != tc.want {
			t.Errorf("parse(%s).String() = %s, want %s", tc.in, got, tc.want)
		}
	}
}

func TestAdd(t *testing.T) {
	testCases := []struct {
		in   []string
		want string
	}{{
		[]string{"[1,1]", "[2,2]", "[3,3]", "[4,4]"},
		"[[[[1,1],[2,2]],[3,3]],[4,4]]",
	}, {
		[]string{"[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"},
		"[[[[3,0],[5,3]],[4,4]],[5,5]]",
	}, {
		[]string{"[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"},
		"[[[[5,0],[7,4]],[5,5]],[6,6]]",
	}, {
		[]string{"[[[[9,8],1],2],3]", "[1,4]"},
		"[[[[0,9],2],3],[1,4]]",
	}, {
		[]string{"[1,1]", "[6,[5,[4,[3,2]]]]"},
		"[[1,1],[6,[5,[7,0]]]]",
	}, {
		[]string{"[6,[5,[4,[3,2]]]]", "[1,1]"},
		"[[6,[5,[7,0]]],[3,1]]",
	}, {
		[]string{"[3,[2,[1,[7,3]]]]", "[6,[5,[4,[3,2]]]]"},
		"[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
	}, {
		[]string{"[3,[2,[8,0]]]", "[9,[5,[4,[3,2]]]]"},
		"[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
	}, {
		[]string{"[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]"},
		"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]",
	}, {
		[]string{
			"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
			"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
			"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
			"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
			"[7,[5,[[3,8],[1,4]]]]",
			"[[2,[2,2]],[8,[8,1]]]",
			"[2,9]",
			"[1,[[[9,3],9],[[9,0],[0,7]]]]",
			"[[[5,[7,4]],7],1]",
			"[[[[4,2],2],6],[8,7]]",
		},
		"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
	}}
	for i, tc := range testCases {
		sum := parse(tc.in[0])
		for _, next := range tc.in[1:] {
			sum = sum.add(parse(next))
		}
		got := sum.String()
		if tc.want != got {
			t.Errorf("%d: sum = %s, want %s", i, got, tc.want)
		}
	}
}

func TestMagnitude(t *testing.T) {
	testCases := []struct {
		in   string
		want int
	}{
		{"[[1,2],[[3,4],5]]", 143},
		{"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384},
		{"[[[[1,1],[2,2]],[3,3]],[4,4]]", 445},
		{"[[[[3,0],[5,3]],[4,4]],[5,5]]", 791},
		{"[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137},
		{"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488},
	}
	for _, tc := range testCases {
		got := parse(tc.in).magnitude()
		if got != tc.want {
			t.Errorf("%s.magnitude() = %d, want %d", tc.in, got, tc.want)
		}
	}
}

func TestDay18(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 4140},
		{FName: "./input.txt", Part1: true, Want: 4469},
	}
	lib.Test(t, testCases, Run)
}
