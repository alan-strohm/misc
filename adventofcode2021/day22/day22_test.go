package day22

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func TestClipTo(t *testing.T) {
	testCases := []struct{ a, b, want region }{{
		a:    region{point{-20, -21, 17}, point{29, 24, 29}},
		b:    region{point{-46, -6, -50}, point{8, 47, 0}},
		want: region{point{-20, -6, 17}, point{8, 24, 17}},
	}}
	for _, tc := range testCases {
		got := tc.a.clipTo(tc.b)
		if got != tc.want {
			t.Errorf("%v.clipTo(%v) = %v, want %v", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestSubtractFrom(t *testing.T) {
	testCases := []struct {
		a, b region
		want []region
	}{{
		a: region{lo: point{0, 0, 0}, hi: point{1, 1, 1}},
		b: region{lo: point{-1, -1, -1}, hi: point{2, 2, 2}},
		want: []region{
			{lo: point{-1, -1, -1}, hi: point{0, 0, 0}},
			{lo: point{-1, -1, 0}, hi: point{0, 0, 1}},
			{lo: point{-1, -1, 1}, hi: point{0, 0, 2}},

			{lo: point{-1, 0, -1}, hi: point{0, 1, 0}},
			{lo: point{-1, 0, 0}, hi: point{0, 1, 1}},
			{lo: point{-1, 0, 1}, hi: point{0, 1, 2}},

			{lo: point{-1, 1, -1}, hi: point{0, 2, 0}},
			{lo: point{-1, 1, 0}, hi: point{0, 2, 1}},
			{lo: point{-1, 1, 1}, hi: point{0, 2, 2}},

			{lo: point{0, -1, -1}, hi: point{1, 0, 0}},
			{lo: point{0, -1, 0}, hi: point{1, 0, 1}},
			{lo: point{0, -1, 1}, hi: point{1, 0, 2}},

			{lo: point{0, 0, -1}, hi: point{1, 1, 0}},
			// {lo: point{0, 0, 0}, hi: point{1, 1, 1}},
			{lo: point{0, 0, 1}, hi: point{1, 1, 2}},

			{lo: point{0, 1, -1}, hi: point{1, 2, 0}},
			{lo: point{0, 1, 0}, hi: point{1, 2, 1}},
			{lo: point{0, 1, 1}, hi: point{1, 2, 2}},

			{lo: point{1, -1, -1}, hi: point{2, 0, 0}},
			{lo: point{1, -1, 0}, hi: point{2, 0, 1}},
			{lo: point{1, -1, 1}, hi: point{2, 0, 2}},

			{lo: point{1, 0, -1}, hi: point{2, 1, 0}},
			{lo: point{1, 0, 0}, hi: point{2, 1, 1}},
			{lo: point{1, 0, 1}, hi: point{2, 1, 2}},

			{lo: point{1, 1, -1}, hi: point{2, 2, 0}},
			{lo: point{1, 1, 0}, hi: point{2, 2, 1}},
			{lo: point{1, 1, 1}, hi: point{2, 2, 2}},
		},
	}, {
		a:    region{lo: point{1, 1, 1}, hi: point{2, 2, 2}},
		b:    region{lo: point{1, 1, 1}, hi: point{2, 2, 2}},
		want: []region{},
	}, {
		a:    region{lo: point{0, 0, 0}, hi: point{3, 3, 3}},
		b:    region{lo: point{1, 1, 1}, hi: point{2, 2, 2}},
		want: []region{},
	}, {
		a:    region{lo: point{0, 0, 0}, hi: point{1, 1, 1}},
		b:    region{lo: point{1, 1, 1}, hi: point{2, 2, 2}},
		want: []region{{lo: point{1, 1, 1}, hi: point{2, 2, 2}}},
	}}
	for _, tc := range testCases {
		got := tc.a.subtractFrom(tc.b)
		if fmt.Sprintf("%v", got) != fmt.Sprintf("%v", tc.want) {
			t.Errorf("%v.subtractFrom(%v) =\n%v\n, want\n%v", tc.a, tc.b, got, tc.want)
		}
		for _, piece := range got {
			if piece.area() < 0 {
				t.Errorf("got negative area")
			}
		}
	}
}

func TestDay22(t *testing.T) {
	testCases := []*lib.TestCase{
		{FName: "./sample.txt", Part1: true, Want: 590784},
		{FName: "./input.txt", Part1: true, Want: 503864},
		{FName: "./sample2.txt", Part1: true, Want: 474140},
		{FName: "./sample2.txt", Part1: false, Want: 2758514936282235},
		{FName: "./input.txt", Part1: false, Want: 1255547543528356},
	}
	lib.Test(t, testCases, Run)
}
