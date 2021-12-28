package day22

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

func max(a, b int) int {
	if b > a {
		return b
	}
	return a
}

func min(a, b int) int {
	if b < a {
		return b
	}
	return a
}

type point struct{ x, y, z int }

func (p point) add(o point) point { return point{p.x + o.x, p.y + o.y, p.z + o.z} }
func (p point) lte(o point) bool  { return p.x <= o.x && p.y <= o.y && p.z <= o.z }
func (p point) max(o point) point { return point{max(p.x, o.x), max(p.y, o.y), max(p.z, o.z)} }
func (p point) min(o point) point { return point{min(p.x, o.x), min(p.y, o.y), min(p.z, o.z)} }

// [lo, hi)
type region struct{ lo, hi point }

func (r region) clipTo(within region) region {
	return region{r.hi.min(r.lo.max(within.lo)), r.lo.max(r.hi.min(within.hi))}
}

func (r region) area() int {
	dx, dy, dz := r.hi.x-r.lo.x, r.hi.y-r.lo.y, r.hi.z-r.lo.z
	return dx * dy * dz
}

func (r region) subtractFrom(o region) (pieces []region) {
	within := r.clipTo(o)
	if within.area() == 0 {
		return []region{o}
	}
	points := []point{o.lo, within.lo, within.hi, o.hi}
	for x := 0; x < 3; x++ {
		for y := 0; y < 3; y++ {
			for z := 0; z < 3; z++ {
				if x == 1 && y == 1 && z == 1 {
					continue
				}
				piece := region{
					lo: point{points[x].x, points[y].y, points[z].z},
					hi: point{points[x+1].x, points[y+1].y, points[z+1].z}}

				if piece.area() == 0 {
					continue
				}
				if piece.area() < 0 {
					lib.Dbg("area: %d when subtracting %v from %v, piece %d, %d, %d\n", piece.area(), o, r, x, y, z)
				}
				pieces = append(pieces, piece)
			}
		}
	}
	return pieces
}

type reactor struct{ regions map[region]bool }

func (r *reactor) setRegion(in region, val bool) {
	for reg, val := range r.regions {
		pieces := in.subtractFrom(reg)
		if len(pieces) == 1 && pieces[0] == reg {
			// No overlap
			continue
		}
		delete(r.regions, reg)
		for _, piece := range pieces {
			r.regions[piece] = val
		}
	}
	if val {
		r.regions[in] = val
	}
}

func (r *reactor) numOn() (n int) {
	for reg, val := range r.regions {
		if val {
			n += reg.area()
		}
	}
	return n
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	r := &reactor{regions: make(map[region]bool)}
	for scanner.Scan() {
		reg := region{}
		var op string
		if _, err := fmt.Sscanf(scanner.Text(), "%s x=%d..%d,y=%d..%d,z=%d..%d", &op, &reg.lo.x, &reg.hi.x, &reg.lo.y, &reg.hi.y, &reg.lo.z, &reg.hi.z); err != nil {
			return 0, err
		}
		reg.hi = reg.hi.add(point{1, 1, 1})
		if p1 && !(reg.hi.lte(point{51, 51, 51}) && point{-50, -50, -50}.lte(reg.lo)) {
			continue
		}
		r.setRegion(reg, op == "on")
	}
	return r.numOn(), nil
}
