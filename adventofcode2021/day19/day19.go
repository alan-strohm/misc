package day19

import (
	"bufio"
	"fmt"
	"math"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func check(cond bool, format string, a ...interface{}) {
	if !cond {
		panic(fmt.Sprintf(format, a...))
	}
}

type point [3]int

func sumOfSquares(a, b point) (r int) {
	for i, _ := range a {
		diff := b[i] - a[i]
		r += diff * diff
	}
	return r
}

func (a point) add(b point) (r point) {
	for i, _ := range a {
		r[i] = a[i] + b[i]
	}
	return r
}

func (a point) scale(s int) (r point) {
	for i, _ := range a {
		r[i] = a[i] * s
	}
	return r
}

func (a point) sub(b point) point { return a.add(b.scale(-1)) }

func crossProduct(a, b []int) []int {
	check(len(a) == 3 && len(b) == 3,
		"can't take the cross product of vectors of lengths %d and %d", len(a), len(b))
	return []int{
		a[1]*b[2] - a[2]*b[1],
		a[2]*b[0] - a[0]*b[2],
		a[0]*b[1] - a[1]*b[0]}
}

func dotProduct(a, b []int) (r int) {
	check(len(a) == len(b), "mismatched lengths: %d vs %d", len(a), len(b))
	for i, _ := range a {
		r += a[i] * b[i]
	}
	return r
}

type matrix struct {
	cells  [][]int
	nr, nc int
}

func zeroMatrix(nr, nc int) *matrix {
	r := &matrix{
		nr: nr, nc: nc,
		cells: make([][]int, nr),
	}
	for row, _ := range r.cells {
		r.cells[row] = make([]int, nc)
	}
	return r
}

func colMatrix(col []int) *matrix {
	r := zeroMatrix(len(col), 1)
	for i, v := range col {
		r.cells[i][0] = v
	}
	return r
}

func (m *matrix) col(ci int) []int {
	r := make([]int, m.nr)
	for ri, row := range m.cells {
		r[ri] = row[ci]
	}
	return r
}

func (m *matrix) mul(o *matrix) *matrix {
	check(o.nr == m.nc,
		"bad dimensions for multiply: [%dx%d].mul([%dx%d])", m.nr, m.nc, o.nr, o.nc)
	res := zeroMatrix(m.nr, o.nc)
	for r := 0; r < res.nr; r++ {
		for c := 0; c < res.nc; c++ {
			res.cells[r][c] = dotProduct(o.col(c), m.cells[r])
		}
	}
	return res
}

func det(m *matrix) int {
	if m.nr == 2 && m.nc == 2 {
		return m.cells[0][0]*m.cells[1][1] - m.cells[0][1]*m.cells[1][0]
	}
	check(m.nr == 3 && m.nc == 3, "can't take the determinant of %d by %d matrix", m.nr, m.nc)

	coeffs := m.cells[0]
	coeffs[1] *= -1

	in := m.cells
	subs := [][][]int{
		{in[1][1:], in[2][1:]},
		{{in[1][0], in[1][2]}, {in[2][0], in[2][2]}},
		{in[1][:2], in[2][:2]}}

	r := 0
	for i, coeff := range coeffs {
		r += coeff * det(&matrix{nr: 2, nc: 2, cells: subs[i]})
	}
	return r
}

func orient(m *matrix, p point) point {
	np := m.mul(colMatrix(p[:])).col(0)
	return point{np[0], np[1], np[2]}
}

type sumToBeacons map[int][][2]point

func (s sumToBeacons) insert(sum int, pair [2]point) {
	s[sum] = append(s[sum], pair)
}

type scanner struct {
	id          int
	beacons     map[point][]int
	sums        sumToBeacons
	offset      *point
	orientation *matrix
}

func (s *scanner) addBeacon(p point) {
	psums := make([]int, 0, len(s.beacons))
	for b, _ := range s.beacons {
		sum := sumOfSquares(b, p)
		s.sums.insert(sum, [2]point{b, p})
		s.beacons[b] = append(s.beacons[b], sum)
		psums = append(psums, sum)
	}
	s.beacons[p] = psums
}

var (
	orientations []*matrix
	identity     = &matrix{
		nr: 3, nc: 3,
		cells: [][]int{
			{1, 0, 0},
			{0, 1, 0},
			{0, 0, 1},
		}}
)

func init() {
	dirs, dims := []int{1, -1}, []int{0, 1, 2}
	for _, dim1 := range dims {
		for _, dim2 := range dims {
			if dim2 == dim1 {
				continue
			}
			for _, dir1 := range dirs {
				for _, dir2 := range dirs {
					m := zeroMatrix(len(dims), len(dims))
					m.cells[dim1][0] = dir1
					m.cells[dim2][1] = dir2
					col2 := crossProduct(m.col(0), m.col(1))
					m.cells[0][2] = col2[0]
					m.cells[1][2] = col2[1]
					m.cells[2][2] = col2[2]
					check(det(m) == 1, "det(%v) = %d", m.cells, det(m))
					orientations = append(orientations, m)
				}
			}
		}
	}
}

func (s *scanner) alignPoint(p point) point {
	return orient(s.orientation, p).add(*s.offset)
}

func verifyAlignment(a, b *scanner, sums map[int]bool) bool {
	mapping := make(map[point]point, 12)
	for sum, _ := range sums {
		for _, apair := range a.sums[sum] {
			for _, abr := range apair {
				if _, ok := mapping[abr]; ok {
					continue
				}
				aba := a.alignPoint(abr)
				for _, bpair := range b.sums[sum] {
					for _, bbr := range bpair {
						if aba == b.alignPoint(bbr) {
							mapping[abr] = bbr
						}
					}
				}
				if _, ok := mapping[abr]; !ok {
					return false
				}
			}
		}
	}
	return true
}

// If the points in as share a common difference with the points in bs, return it.
// Otherwise, return nil.
func commonDiff(as, bs [2]point) *point {
	for i, b := range bs {
		diff := as[0].sub(b)
		if as[1].sub(bs[i^1]) == diff {
			return &diff
		}
	}
	return nil
}

// Attempt to align s to o.  Returns false if s does not share 12 beacons with o.
func (s *scanner) align(o *scanner) bool {
	common := map[int]bool{}
	for sum, _ := range s.sums {
		if _, ok := o.sums[sum]; ok {
			common[sum] = true
		}
	}
	if len(common) < 12*11/2 {
		return false
	}
	for _, orientation := range orientations {
		for sum, _ := range common {
			for _, sbs := range s.sums[sum] {
				for _, obs := range o.sums[sum] {
					oobs := [2]point{orient(orientation, obs[0]), orient(orientation, obs[1])}
					if diff := commonDiff(sbs, oobs); diff != nil {
						offset := orient(s.orientation, *diff).add(*s.offset)
						o.offset = &offset
						o.orientation = s.orientation.mul(orientation)
						if verifyAlignment(s, o, common) {
							lib.Dbg("aligned scanner %d to scanner %d with offset: %d\n", o.id, s.id, *o.offset)
							lib.Dbg("anchor %v translated to %v\n", obs[0], o.alignPoint(obs[0]))
							return true
						}
						o.offset, o.orientation = nil, nil
					}
				}
			}
		}
	}
	return false
}

func alignScanners(scanners []*scanner) {
	scanners[0].offset = &point{}
	scanners[0].orientation = identity
	aligned := []*scanner{scanners[0]}
	for len(aligned) < len(scanners) {
		before := len(aligned)
		for _, from := range scanners {
			if from.orientation != nil {
				continue
			}
			for _, to := range aligned {
				if to.align(from) {
					aligned = append(aligned, from)
					break
				}
			}
		}
		check(len(aligned) != before, "failed to align any new scanners")
	}
}

func countBeacons(scanners []*scanner) int {
	beacons := make(map[point]bool)
	for _, s := range scanners {
		for b, _ := range s.beacons {
			beacons[s.alignPoint(b)] = true
		}
	}
	return len(beacons)
}

func manDist(a, b point) (dist int) {
	for i, _ := range a {
		dist += int(math.Abs(float64(a[i] - b[i])))
	}
	return dist
}

func maxDistance(scanners []*scanner) (max int) {
	for _, s1 := range scanners {
		for _, s2 := range scanners {
			dist := manDist(*s1.offset, *s2.offset)
			if dist > max {
				max = dist
			}
		}
	}
	return max
}

func Run(s *bufio.Scanner, p1 bool) (int, error) {
	scanners := []*scanner{}
	for s.Scan() {
		sc := scanner{id: len(scanners), sums: sumToBeacons{}, beacons: map[point][]int{}}
		for s.Scan() && s.Text() != "" {
			var p point
			if _, err := fmt.Sscanf(s.Text(), "%d,%d,%d", &p[0], &p[1], &p[2]); err != nil {
				return 0, err
			}
			sc.addBeacon(p)
		}
		scanners = append(scanners, &sc)
	}
	alignScanners(scanners)
	if p1 {
		return countBeacons(scanners), nil
	}
	return maxDistance(scanners), nil
}
