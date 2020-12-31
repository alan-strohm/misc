package day24

import (
	"fmt"
	"log"

	"../io"
)

type hex struct {
	q, r int
}

type dir hex

var (
	eDir  = dir(hex{1, 0})
	seDir = dir(hex{0, 1})
	swDir = dir(hex{-1, 1})
	wDir  = dir(hex{-1, 0})
	nwDir = dir(hex{0, -1})
	neDir = dir(hex{1, -1})

	dirs = []dir{
		eDir, seDir, swDir, wDir, nwDir, neDir,
	}
)

func (h hex) add(d dir) hex {
	return hex{q: h.q + d.q, r: h.r + d.r}
}

func (d dir) scale(s int) dir {
	return dir{q: d.q * s, r: d.r * s}
}

func (h hex) String() string {
	return fmt.Sprintf("%d,%d", h.q, h.r)
}

func fromPath(s string) (r hex, err error) {
	r = hex{q: 0, r: 0}
	p := []byte(s)
	for i := 0; i < len(p); i++ {
		d := string(p[i])
		if d == "n" || d == "s" {
			i++
			d += string(p[i])
		}
		switch d {
		case "e":
			r = r.add(eDir)
		case "w":
			r = r.add(wDir)
		case "nw":
			r = r.add(nwDir)
		case "se":
			r = r.add(seDir)
		case "ne":
			r = r.add(neDir)
		case "sw":
			r = r.add(swDir)
		default:
			err = fmt.Errorf("unexpected path: %s", s)
			return
		}
	}
	// log.Printf("path [%s] to %s", s, r)
	return
}

type grid map[string]bool

func fromBlackHex(b []hex) *grid {
	g := &grid{}
	for _, h := range b {
		g.toggle(h)
	}
	return g
}

func (g *grid) ScanLine(l string) (done bool, err error) {
	h, err := fromPath(l)
	if err != nil {
		return
	}
	g.toggle(h)
	return
}

func fromFile(fname string) (r *grid, err error) {
	r = &grid{}
	if err = io.Scan(r, fname); err != nil {
		return
	}
	return
}

func (g *grid) String() string {
	return fmt.Sprint(*g)
}

func (g *grid) numBlack() int {
	return len(*g)
}

func (g *grid) get(h hex) bool {
	return (*g)[h.String()]
}

func (g *grid) toggle(h hex) {
	if g.get(h) {
		delete(*g, h.String())
	} else {
		(*g)[h.String()] = true
	}
}

func (g *grid) nextState(h hex) bool {
	numBlackNeighbors := 0
	for _, d := range dirs {
		if g.get(h.add(d)) {
			numBlackNeighbors++
		}
	}
	isBlack := g.get(h)
	stayBlack := (isBlack && numBlackNeighbors > 0 && numBlackNeighbors < 3)
	turnBlack := (!isBlack && numBlackNeighbors == 2)
	return stayBlack || turnBlack
}

func (g *grid) step() (newGrid *grid, numBlack int) {
	newGrid = &grid{}
	c := hex{0, 0}
	if g.nextState(c) {
		newGrid.toggle(c)
	}
	numSeen := 0
	if g.get(c) {
		numSeen++
	}
	for r := 1; numBlack != len(*newGrid) || numSeen != g.numBlack(); r++ {
		numBlack = len(*newGrid)
		curr := c.add(nwDir.scale(r))
		for _, d := range dirs {
			for i := 0; i < r; i++ {
				if g.nextState(curr) {
					newGrid.toggle(curr)
				}
				if g.get(curr) {
					numSeen++
				}
				curr = curr.add(d)
			}
		}
	}
	return
}

func part1(fname string) (int, error) {
	g, err := fromFile(fname)
	if err != nil {
		return 0, err
	}
	return g.numBlack(), nil
}

func part2(fname string) (int, error) {
	g, err := fromFile(fname)
	if err != nil {
		return 0, err
	}
	numBlack := 0
	for i := 0; i < 100; i++ {
		g, numBlack = g.step()
		if i%10+1 == 10 {
			log.Printf("Day %d: %d", i+1, numBlack)
		}
	}
	return g.numBlack(), nil
}
