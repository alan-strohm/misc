package day20

import (
	"testing"
)

func TestPart1(t *testing.T) {
	testCases := []struct {
		fname string
		want  int
	}{
		// {fname: "./ex1.txt", want: 20899048083289},
		{fname: "./input.txt", want: 5966506063747},
	}
	for _, tc := range testCases {
		n, err := part1(tc.fname)
		if err != nil {
			t.Errorf("failed: %s", err)
		}
		if n != tc.want {
			t.Errorf("got %d, want %d", n, tc.want)
		}
	}
}

var sub3x3 = &photo{
	w: 3, h: 3, pixels: [][]rune{
		[]rune("123"), []rune("456"), []rune("789"),
	}}
var sub3x3WantLeft = &photo{
	w: 15, h: 7, pixels: [][]rune{
		[]rune("123 789 321 987"),
		[]rune("456 456 654 654"),
		[]rune("789 123 987 321"),
		[]rune("               "),
		[]rune("741 963 147 369"),
		[]rune("852 852 258 258"),
		[]rune("963 741 369 147"),
	}}
var sub3x3WantTop = &photo{
	w: 15, h: 7, pixels: [][]rune{
		[]rune("123 147 789 369"),
		[]rune("456 258 456 258"),
		[]rune("789 369 123 147"),
		[]rune("               "),
		[]rune("321 741 987 963"),
		[]rune("654 852 654 852"),
		[]rune("987 963 321 741"),
	}}

var sub4x4 = &photo{
	w: 4, h: 4, pixels: [][]rune{
		[]rune("1234"), []rune("5678"), []rune("9ABC"), []rune("DEFG"),
	}}

var sub4x4WantLeft = &photo{
	w: 19, h: 9, pixels: [][]rune{
		[]rune("1234 DEFG 4321 GFED"),
		[]rune("5678 9ABC 8765 CBA9"),
		[]rune("9ABC 5678 CBA9 8765"),
		[]rune("DEFG 1234 GFED 4321"),
		[]rune("                   "),
		[]rune("D951 GC84 159D 48CG"),
		[]rune("EA62 FB73 26AE 37BF"),
		[]rune("FB73 EA62 37BF 26AE"),
		[]rune("GC84 D951 48CG 159D"),
	}}

var monsterWantLeft = &photo{
	w: 42, h: 21, pixels: [][]rune{
		[]rune("                  #   #  #  #  #  #  #     #                      #  #  #  #  #  # "),
		[]rune("#    ##    ##    ### #    ##    ##    ### ###    ##    ##    # ###    ##    ##    #"),
		[]rune(" #  #  #  #  #  #                      #     #  #  #  #  #  #   #                  "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune("                                                                                   "),
		[]rune(" #                    #                    #                    #                  "),
		[]rune("#                     ##                    #                  ##                  "),
		[]rune("                      #                                         #                  "),
		[]rune("                     #                                           #                 "),
		[]rune("#                                           #                                      "),
		[]rune(" #                                         #                                       "),
		[]rune(" #                   #                     #                     #                 "),
		[]rune("#                     #                     #                   #                  "),
		[]rune("                      #                                         #                  "),
		[]rune("                     #                                           #                 "),
		[]rune("#                                           #                                      "),
		[]rune(" #                                         #                                       "),
		[]rune(" #                   #                     #                     #                 "),
		[]rune("#                     #                     #                   #                  "),
		[]rune("                      #                                         #                  "),
		[]rune("                     #                                           #                 "),
		[]rune("#                                           #                                      "),
		[]rune(" #                                         #                                       "),
		[]rune(" ##                  #                    ##                     #                 "),
		[]rune(" #                    #                    #                    #                  "),
	}}

func TestSideToLeft(t *testing.T) {
	testCases := []struct {
		in, want *photo
	}{
		{in: sub3x3, want: sub3x3WantLeft},
		{in: sub4x4, want: sub4x4WantLeft},
		{in: monster, want: monsterWantLeft},
	}
	for _, tc := range testCases {
		sub := tc.in
		dim := sub.w
		if sub.h > dim {

		}
		canvas := blankPhoto(4*dim+3, 2*dim+1)
		for i, s := range []photoSide{leftSide, leftRevSide, rightSide, rightRevSide} {
			sub.applyAll(sideToLeft[s]).paint(canvas, dim*i+i, 0)
		}
		for i, s := range []photoSide{bottomSide, bottomRevSide, topSide, topRevSide} {
			sub.applyAll(sideToLeft[s]).paint(canvas, dim*i+i, dim+1)
		}
		if canvas.String() != tc.want.String() {
			t.Errorf("Got:\n%s\nWant:\n%s", canvas, tc.want)
		}
	}
}

func TestSideToTop(t *testing.T) {
	testCases := []struct {
		in, want *photo
	}{
		{in: sub3x3, want: sub3x3WantTop},
	}
	for _, tc := range testCases {
		sub := tc.in
		dim := sub.w
		if sub.h > dim {

		}
		canvas := blankPhoto(4*dim+3, 2*dim+1)
		for i, s := range []photoSide{topSide, leftSide, bottomSide, rightSide} {
			sub.applyAll(sideToTop[s]).paint(canvas, dim*i+i, 0)
		}
		for i, s := range []photoSide{topRevSide, leftRevSide, bottomRevSide, rightRevSide} {
			sub.applyAll(sideToTop[s]).paint(canvas, dim*i+i, dim+1)
		}
		if canvas.String() != tc.want.String() {
			t.Errorf("Got:\n%s\nWant:\n%s", canvas, tc.want)
		}
	}
}
