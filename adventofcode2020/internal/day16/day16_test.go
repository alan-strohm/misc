package day16

import (
	"reflect"
	"testing"
)

func TestParseUnion(t *testing.T) {
	testCases := []struct {
		in   string
		want string
		err  error
	}{
		{in: "", want: "", err: nil},
		{in: "1-3", want: "1-3", err: nil},
		{in: "1-3,2-4", want: "1-4", err: nil},
		{in: "5-6,1-3", want: "1-3,5-6", err: nil},
		{in: "2-4,1-3", want: "1-4", err: nil},
		{in: "1-3,0-6", want: "0-6", err: nil},
		{in: "1-2,5-6,3-4", want: "1-2,3-4,5-6", err: nil},
		{in: "1-3,4-6,2-5", want: "1-6", err: nil},
		{in: "1-3,5-7,6-11,33-44", want: "1-3,5-11,33-44", err: nil},
	}
	for _, tc := range testCases {
		u, err := parseUnion(tc.in)
		if (err == nil) != (tc.err == nil) || u.String() != tc.want {
			t.Errorf("parseUnion(%s) = %s, %v, want %s, %v", tc.in, u, err, tc.want, tc.err)
		}
	}
}

func TestFindEntry(t *testing.T) {
	testCases := []struct {
		union        string
		num, wantLoc int
		want         *interval
	}{
		{union: "", num: 1, wantLoc: 0, want: nil},
		{union: "1-3", num: 2, wantLoc: 0, want: &interval{min: 1, max: 3}},
		{union: "1-3", num: 1, wantLoc: 0, want: &interval{min: 1, max: 3}},
		{union: "1-3", num: 3, wantLoc: 0, want: &interval{min: 1, max: 3}},
		{union: "1-3", num: 0, wantLoc: 0, want: nil},
		{union: "1-3", num: 4, wantLoc: 1, want: nil},
		{union: "5-6", num: 1, wantLoc: 0, want: nil},
		{union: "1-3,5-6", num: 4, wantLoc: 1, want: nil},
		{union: "1-3,5-6", num: 7, wantLoc: 2, want: nil},
		{union: "1-3,5-6", num: 6, wantLoc: 1, want: &interval{min: 5, max: 6}},
	}
	for _, tc := range testCases {
		u, err := parseUnion(tc.union)
		if err != nil {
			t.Errorf("failed to parse %s: %s", tc.union, err)
		}
		loc, i := u.findEntry(tc.num)
		if loc != tc.wantLoc || !i.Equals(tc.want) {
			t.Errorf("%s.findEntry(%d) = %d, %v, want %d, %v", tc.union, tc.num, loc, i, tc.wantLoc, tc.want)
		}
	}
}

func TestErrorRate(t *testing.T) {
	in, err := ParseInput("./ex1.txt")
	if err != nil {
		t.Errorf("could not parse input: %s", err)
		return
	}
	rate := ErrorRate(in)
	if rate != 71 {
		t.Errorf("got %d, wanted 71", rate)
	}
}

func TestFieldIndices(t *testing.T) {
	in, err := ParseInput("./ex2.txt")
	if err != nil {
		t.Errorf("could not parse input: %s", err)
		return
	}
	indices := in.fieldIndices()
	want := map[string]int{
		"row": 0, "class": 1, "seat": 2,
	}
	if !reflect.DeepEqual(indices, want) {
		t.Errorf("got %v, want %v", indices, want)
	}
}
