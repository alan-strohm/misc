package day04

import (
	"bufio"
	"strconv"
	"strings"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type cell struct {
	val    int
	marked bool
}

type cellRef struct {
	b    *board
	r, c int
}

type board struct {
	id  int
	cs  [5][5]cell
	won bool
}

func (b *board) mark(r, c int) bool {
	b.cs[r][c].marked = true
	rows := 0
	for _, row := range b.cs {
		if row[c].marked {
			rows++
		}
	}
	cols := 0
	for _, cell := range b.cs[r] {
		if cell.marked {
			cols++
		}
	}
	// lib.Dbg("  r: %d, c: %d, rm: %d, cm: %d\n", r, c, rows, cols)
	if !b.won && (rows == len(b.cs) || cols == len(b.cs[0])) {
		lib.Dbg("board %d won!\n", b.id)
		b.won = true
		return true
	}
	return false
}

func (b *board) score() int {
	r := 0
	for _, row := range b.cs {
		for _, cell := range row {
			if !cell.marked {
				r += cell.val
			}
		}
	}
	return r
}

type index map[int][]*cellRef

func (i index) call(n int) []*board {
	lib.Dbg("%d called, %d boards:\n", n, len(i[n]))
	r := make([]*board, 0)
	for _, ref := range i[n] {
		if ref.b.mark(ref.r, ref.c) {
			r = append(r, ref.b)
		}
	}
	return r
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	scanner.Scan()
	draws := strings.Split(scanner.Text(), ",")
	idx := index(make(map[int][]*cellRef))
	numBoards := 0
	for scanner.Scan() {
		b := &board{id: numBoards}
		numBoards++
		for i := 0; i < len(b.cs) && scanner.Scan(); i++ {
			cs := strings.Fields(scanner.Text())
			for j, c := range cs {
				n, err := strconv.Atoi(c)
				if err != nil {
					return 0, err
				}
				b.cs[i][j].val = n
				idx[n] = append(idx[n], &cellRef{b: b, r: i, c: j})
			}
		}
	}
	lib.Dbg("Starting with %d boards.", numBoards)
	for _, draw := range draws {
		n, err := strconv.Atoi(draw)
		if err != nil {
			return 0, err
		}
		if bs := idx.call(n); len(bs) != 0 {
			if p1 {
				return n * bs[0].score(), nil
			}
			numBoards -= len(bs)
			lib.Dbg("Remaining boards: %d\n", numBoards)
			if numBoards == 0 {
				return n * bs[len(bs)-1].score(), nil
			}
		}
	}
	return 0, nil
}
