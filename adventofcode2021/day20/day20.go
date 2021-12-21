package day20

import (
	"bufio"
	"fmt"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type pxl byte

func (p pxl) on() bool {
	return p == '#'
}

type input struct {
	algo [512]pxl
	img  [][]pxl
	def  pxl
}

func (i *input) String() string {
	var sb strings.Builder
	for _, row := range i.img {
		fmt.Fprintf(&sb, "%s\n", row)
	}
	return sb.String()
}

func (i *input) getPixel(x, y int) pxl {
	if y >= 0 && y < len(i.img) && x >= 0 && x < len(i.img[y]) {
		return i.img[y][x]
	}
	return i.def
}

func (i *input) nextPixel(x, y int) pxl {
	idx := 0
	for xi, xoff := range []int{1, 0, -1} {
		for yi, yoff := range []int{1, 0, -1} {
			if i.getPixel(x+xoff, y+yoff).on() {
				bit := yi*3 + xi
				idx |= 1 << bit
			}
		}
	}
	return i.algo[idx]
}

func (i *input) enhance() {
	newImg := make([][]pxl, len(i.img)+2)
	for ny, _ := range newImg {
		newImg[ny] = make([]pxl, len(i.img[0])+2)
		for nx, _ := range newImg[ny] {
			newImg[ny][nx] = i.nextPixel(nx-1, ny-1)
		}
	}
	i.img = newImg

	if i.def.on() {
		i.def = i.algo[512-1]
	} else {
		i.def = i.algo[0]
	}
}

func (i *input) numLit() int {
	cnt := 0
	for _, row := range i.img {
		for _, p := range row {
			if p.on() {
				cnt++
			}
		}
	}
	return cnt
}

func (in *input) Part1() int {
	for i := 0; i < 2; i++ {
		in.enhance()
		lib.Dbg("after enhance: %d by %d img:\n%s", len(in.img[0]), len(in.img), in)
	}
	return in.numLit()
}

func (in *input) Part2() int {
	for i := 0; i < 50; i++ {
		in.enhance()
	}
	lib.Dbg("final image:\n%s", in)
	return in.numLit()
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	r := &input{def: '.'}
	scanner.Scan()
	for i, p := range []pxl(scanner.Text()) {
		r.algo[i] = p
	}
	scanner.Scan() // Blank line
	for scanner.Scan() {
		r.img = append(r.img, []pxl(scanner.Text()))
	}
	return r, nil
}
