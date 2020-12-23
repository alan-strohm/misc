package day21

import "testing"

func TestRun(t *testing.T) {
	testCases := []struct {
		fname     string
		wantPart1 int
		wantPart2 string
	}{
		{fname: "./ex1.txt", wantPart1: 5, wantPart2: "mxmxvkd,sqjhc,fvjkl"},
		{fname: "./input.txt", wantPart1: 2162, wantPart2: "lmzg,cxk,bsqh,bdvmx,cpbzbx,drbm,cfnt,kqprv"},
	}
	for _, tc := range testCases {
		dbg(tc.fname)
		part1, part2, err := run(tc.fname)
		if err != nil {
			t.Errorf("run(%s) failed: %s", tc.fname, err)
		}
		if part1 != tc.wantPart1 || part2 != tc.wantPart2 {
			t.Errorf("run(%s) = %d, [%s]  want: %d, [%s]", tc.fname, part1, part2, tc.wantPart1, tc.wantPart2)
		}
	}
}
