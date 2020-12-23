package day20

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func dbg(in string) {
	if *dbgFlag {
		fmt.Println(in)
	}
}

type photo struct {
	id     int
	h, w   int
	pixels [][]rune
}

var monster = &photo{
	id: 42, w: 20, h: 3,
	pixels: [][]rune{
		[]rune("                  # "),
		[]rune("#    ##    ##    ###"),
		[]rune(" #  #  #  #  #  #   "),
	}}

func blankPhoto(w, h int) *photo {
	r := &photo{id: 3, w: w, h: h, pixels: make([][]rune, h)}
	for y, _ := range r.pixels {
		r.pixels[y] = make([]rune, w)
		for x, _ := range r.pixels[y] {
			r.pixels[y][x] = ' '
		}
	}
	return r
}

func (p photo) String() string {
	strs := make([]string, len(p.pixels))
	for i, l := range p.pixels {
		strs[i] = string(l)
	}
	return strings.Join(strs, "\n")
}

func (p photo) paint(o *photo, ox, oy int) {
	for y, row := range p.pixels {
		for x, pxl := range row {
			o.pixels[y+oy][x+ox] = pxl
		}
	}
}

func (p *photo) row(r int) string {
	if r >= p.h {
		log.Fatalf("oob row access %d vs %d", r, p.h)
	}
	return string(p.pixels[r])
}

func (p *photo) col(c int) string {
	if c >= p.w {
		log.Fatalf("oob col access %d vs %d", c, p.w)
	}
	r := make([]rune, p.h)
	for i, row := range p.pixels {
		r[i] = row[c]
	}
	return string(r)
}

func (p *photo) side(s photoSide) string {
	switch s {
	case topSide:
		return p.row(0)
	case bottomSide:
		return p.row(p.h - 1)
	case rightSide:
		return p.col(p.w - 1)
	case leftSide:
		return p.col(0)
	case topRevSide:
		return reverse(p.side(topSide))
	case bottomRevSide:
		return reverse(p.side(bottomSide))
	case rightRevSide:
		return reverse(p.side(rightSide))
	case leftRevSide:
		return reverse(p.side(leftSide))
	}
	return ""
}

type scanner struct {
	curr   *photo
	photos []*photo
}

var idRE = regexp.MustCompile(`^Tile (\d+):$`)

func readPhotos(fname string) ([]*photo, error) {
	f, err := os.Open(fname)
	if err != nil {
		return nil, fmt.Errorf("opening %s: %s", fname, err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	r := make([]*photo, 0)
	for scanner.Scan() {
		idParts := idRE.FindStringSubmatch(scanner.Text())
		if len(idParts) != 2 {
			return nil, fmt.Errorf("invalid ID line: %s", scanner.Text())
		}
		id, err := strconv.Atoi(idParts[1])
		if err != nil {
			return nil, fmt.Errorf("invalid ID: %s", err)
		}
		if !scanner.Scan() {
			return nil, errors.New("unexpected EOF")
		}
		l := scanner.Text()
		p := &photo{id: id, w: len(l), pixels: [][]rune{[]rune(l)}}
		for scanner.Scan() && scanner.Text() != "" {
			l := scanner.Text()
			if len(l) != p.w {
				return nil, fmt.Errorf("non-square photo %d vs %d", len(l), p.w)
			}
			p.pixels = append(p.pixels, []rune(l))
		}
		p.h = len(p.pixels)
		r = append(r, p)
	}
	return r, nil
}

func reverse(in string) string {
	out := make([]rune, len(in))
	for i, r := range in {
		out[len(in)-i-1] = r
	}
	return string(out)
}

type photoSide string

const (
	topSide       photoSide = "T"
	topRevSide    photoSide = "TR"
	rightSide     photoSide = "R"
	rightRevSide  photoSide = "RR"
	bottomSide    photoSide = "B"
	bottomRevSide photoSide = "BR"
	leftSide      photoSide = "L"
	leftRevSide   photoSide = "LR"
)

var allSides = []photoSide{
	topSide, topRevSide, rightSide, rightRevSide,
	bottomSide, bottomRevSide, leftSide, leftRevSide}

var sideOrder = []photoSide{topSide, rightSide, bottomSide, leftSide}

type transOp string

const (
	cw90 transOp = "cw90"
	flip         = "flip"
)

// side -> set of transforms to put that side on the left.
var sideToLeft = map[photoSide][]transOp{
	topSide:       []transOp{cw90, flip},
	topRevSide:    []transOp{cw90, cw90, cw90},
	rightSide:     []transOp{flip},
	rightRevSide:  []transOp{cw90, cw90},
	bottomSide:    []transOp{cw90},
	bottomRevSide: []transOp{cw90, cw90, cw90, flip},
	leftSide:      []transOp{},
	leftRevSide:   []transOp{cw90, cw90, flip},
}

var sideToTop = map[photoSide][]transOp{
	topSide:       []transOp{},
	topRevSide:    []transOp{cw90, cw90, flip},
	rightSide:     []transOp{cw90, flip},
	rightRevSide:  []transOp{cw90},
	bottomSide:    []transOp{flip},
	bottomRevSide: []transOp{cw90, cw90},
	leftSide:      []transOp{cw90, cw90, cw90},
	leftRevSide:   []transOp{cw90, cw90, cw90, flip},
}

var transToMatrix = map[transOp][][]int{
	cw90: [][]int{[]int{0, -1}, []int{1, 0}},
	flip: [][]int{[]int{-1, 0}, []int{0, 1}},
}

func (t transOp) apply(x, y int) (xt, yt int) {
	matrix := transToMatrix[t]
	xt = x*matrix[0][0] + y*matrix[0][1]
	yt = x*matrix[1][0] + y*matrix[1][1]
	return
}

func applyAll(ops []transOp, x, y, w, h int) (xt, yt int) {
	offx, offy := w/-2, h/-2
	xt, yt = x+offx, y+offy
	for _, op := range ops {
		xt, yt = op.apply(xt, yt)
		if op == cw90 {
			offx, offy = offy, offx
			w, h = h, w
		}
		xt -= (w + 1) % 2
	}
	xt -= offx
	yt -= offy
	return
}

func (p *photo) applyAll(ops []transOp) *photo {
	num90 := 0
	for _, op := range ops {
		if op == cw90 {
			num90++
		}
	}
	w, h := p.w, p.h
	if num90%2 == 1 {
		w, h = p.h, p.w
	}
	r := &photo{id: p.id, w: w, h: h, pixels: make([][]rune, h)}
	for y, _ := range r.pixels {
		r.pixels[y] = make([]rune, w)
	}
	for y, row := range p.pixels {
		for x, pxl := range row {
			xt, yt := applyAll(ops, x, y, p.w, p.h)
			r.pixels[yt][xt] = pxl
		}
	}
	return r
}

type sideRef struct {
	photo *photo
	side  photoSide
}

type index struct {
	photos  map[int]*photo
	sides   map[string][]*sideRef
	corners []*photo
}

func newIndex(photos []*photo) (*index, error) {
	r := &index{
		photos:  make(map[int]*photo),
		sides:   make(map[string][]*sideRef),
		corners: make([]*photo, 0, 4)}
	numDups := 0
	for _, p := range photos {
		r.photos[p.id] = p
		for _, s := range allSides {
			ps := p.side(s)
			r.sides[ps] = append(r.sides[ps], &sideRef{photo: p, side: s})
			if len(r.sides[ps]) > 2 {
				numDups++
			}
		}
	}

	if numDups > 0 {
		return nil, errors.New("ambiguous sides!")
	}
	// photo ID -> 2 * #-of-unmached-edges. The number is doubled because we index
	// by both forward and reversed edges.
	numUnmatched := make(map[int]int)
	for _, v := range r.sides {
		if len(v) == 1 {
			numUnmatched[v[0].photo.id]++
		}
	}
	for k, v := range numUnmatched {
		if v == 4 {
			r.corners = append(r.corners, r.photos[k])
		}
	}
	if len(r.corners) != 4 {
		return nil, fmt.Errorf("got %d corners: %v", len(r.corners), r.corners)
	}
	return r, nil
}

func (idx *index) isEdge(p *photo, s photoSide) bool {
	return idx.getNeighbor(p, s) == nil
}

func (idx *index) getNeighbor(p *photo, s photoSide) *sideRef {
	for _, r := range idx.sides[p.side(s)] {
		if r.photo.id != p.id {
			return r
		}
	}
	return nil
}

func (idx *index) getFirstEdge(p *photo) int {
	for i, s := range sideOrder {
		nexti := (i + 1) % len(sideOrder)
		if !idx.isEdge(p, s) && idx.isEdge(p, sideOrder[nexti]) {
			return nexti
		}
	}
	return -1
}

func (idx *index) compose() (*photo, error) {
	composed := make([][]*photo, 0)
	w, h := 0, 0
	upperLeft := idx.corners[0]
	newLeft := sideOrder[idx.getFirstEdge(upperLeft)]
	upperLeft = upperLeft.applyAll(sideToLeft[newLeft])
	dbg(fmt.Sprintf("Tile %d, side %s\n%s", upperLeft.id, newLeft, upperLeft))
	w += upperLeft.w
	for curr, currRow := upperLeft, 0; !idx.isEdge(curr, bottomSide); currRow++ {
		if currRow != 0 {
			next := idx.getNeighbor(composed[currRow-1][0], bottomSide)
			curr = next.photo.applyAll(sideToTop[next.side])
			dbg(fmt.Sprintf("Tile %d, side %s\n%s", curr.id, next.side, curr))
		}
		h += curr.h
		composed = append(composed, []*photo{curr})
		for !idx.isEdge(curr, rightSide) {
			if currRow == 0 {
				w += curr.w
			}
			next := idx.getNeighbor(curr, rightSide)
			curr = next.photo.applyAll(sideToLeft[next.side])
			dbg(fmt.Sprintf("Tile %d, side %s\n%s", curr.id, next.side, curr))
			composed[currRow] = append(composed[currRow], curr)
		}
	}
	for _, r := range composed {
		for _, c := range r {
			fmt.Printf("%d ", c.id)
		}
		fmt.Println()
	}
	return nil, nil
}

func part1(fname string) (int, error) {
	photos, err := readPhotos(fname)
	if err != nil {
		return 0, err
	}
	idx, err := newIndex(photos)
	if err != nil {
		return 0, err
	}
	idx.compose()
	return idx.corners[0].id * idx.corners[1].id * idx.corners[2].id * idx.corners[3].id, nil
}
