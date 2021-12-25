package day23

import (
	"fmt"
	"math"
)

type state struct {
	hallway [7]cell
	rooms   [4][]cell
	emptied [4]bool
	moves   [4]int
	size    int
	parent  *state
}

type cell byte

var empty cell

func (c cell) isEmpty() bool {
	return c == empty
}
func (c cell) id() int {
	return int(c - 'A')
}
func fromID(id int) cell {
	return cell('A' + id)
}

func (s *state) Format(f fmt.State, verb rune) {
	if s.parent != nil {
		fmt.Fprintf(f, "%v\n", s.parent)
	}
	fmt.Fprint(f, "#############\n#")
	for hi, c := range s.hallway {
		if hi > 1 && hi < 6 {
			fmt.Fprint(f, "|")
		}
		if c.isEmpty() {
			fmt.Fprint(f, ".")
		} else {
			fmt.Fprintf(f, "%c", c)
		}
	}
	fmt.Fprint(f, "#\n")
	for i := 0; i < s.size; i++ {
		fpad := "  #"
		bpad := "\n"
		if i == 0 {
			fpad = "###"
			bpad = "##\n"
		}
		fmt.Fprint(f, fpad)
		for _, room := range s.rooms {
			ni := i - (s.size - len(room))
			if ni < 0 {
				fmt.Fprintf(f, ".#")
			} else {
				fmt.Fprintf(f, "%c#", room[ni])
			}
		}
		fmt.Fprint(f, bpad)
	}
	fmt.Fprint(f, "  #########\n")
	fmt.Fprintf(f, "moves: %v\n", s.moves)
	fmt.Fprintf(f, "emptied: %v\n", s.emptied)
}

var costs = [4]int{1, 10, 100, 1000}
var roomDists = [4][7]int{
	{3, 2, 2, 4, 6, 8, 9},
	{5, 4, 2, 2, 4, 6, 7},
	{7, 6, 4, 2, 2, 4, 5},
	{9, 8, 6, 4, 2, 2, 3},
}

func (s *state) clone() *state {
	r := *s
	r.parent = s
	for ri, room := range s.rooms {
		r.rooms[ri] = make([]cell, len(room))
		copy(r.rooms[ri], room)
	}
	return &r
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// Return true if travel is possible between the entrance to room ri and
// position hi in the hallway
func (s *state) canTravel(ri, hi int) bool {
	rightSide := ri + 2
	from, to := hi+1, rightSide
	if from > to {
		from, to = rightSide, hi
	}
	for i := from; i < to; i++ {
		if !s.hallway[i].isEmpty() {
			return false
		}
	}
	return true
}

// Detect newly emptied rooms. For all emptied rooms, return all the amphs in
// the hallway to that room.
func (s *state) returnAmphs() {
	for progress := true; progress; {
		progress = false
		for ri, room := range s.rooms {
			if len(room) == 0 || allHome(ri, room) {
				s.emptied[ri] = true
			}
			if len(room) >= s.size || !s.emptied[ri] {
				continue
			}
			// The room only contains the correct amphs. Check to see if any can be moved in.
			for hi, cell := range s.hallway {
				if cell != fromID(ri) {
					continue
				}
				if !s.canTravel(ri, hi) {
					continue
				}
				progress = true
				s.moves[ri] += roomDists[ri][hi] - 1 + s.size - len(s.rooms[ri])
				s.rooms[ri] = append(s.rooms[ri], cell)
				s.hallway[hi] = empty
				// TODO: perhaps check that if this is the last amph to return, there are
				// no more out there.
			}
		}
	}
}

// If the state represents a win, return the cost.
func (s *state) hasWin() (bool, int) {
	for ri, room := range s.rooms {
		if !s.emptied[ri] || len(room) != s.size {
			return false, -1
		}
	}
	cost := 0
	for ri, moves := range s.moves {
		cost += moves * costs[ri]
	}
	return true, cost
}

// Are all the ampths in room home?
func allHome(ri int, room []cell) bool {
	homeAmph := fromID(ri)
	for _, amph := range room {
		if amph != homeAmph {
			return false
		}
	}
	return true
}

var (
	// path  = []int{27, 18, 14, 11, 6, 1}
	// path  = []int{27, 18, 14, 11, 6, 3, 3, 3, 1, 1, 2, 0, 2, 2}
	path  = []int{}
	pathi = 0
	debug = false
)

// Clone new states for each possible way one amph could leave each room.
func (s *state) nextMoves() []*state {
	r := make([]*state, 0)
	for ri, room := range s.rooms {
		if s.emptied[ri] {
			continue // Stop trying to remove amphs from a room once it is emptied.
		}
		amph := room[0]
		for hi, cell := range s.hallway {
			if !cell.isEmpty() || !s.canTravel(ri, hi) {
				continue
			}
			next := s.clone()
			next.hallway[hi] = amph
			next.moves[amph.id()] += s.size - len(s.rooms[ri]) + roomDists[ri][hi]
			next.rooms[ri] = next.rooms[ri][1:]
			r = append(r, next)
		}
	}

	// Debugging support. Force the last entry to follow the requested path and
	// turn on debugging at the end of the path.
	if pathi < len(path) {
		to, from := len(r)-1, path[pathi]
		if from != to {
			r[to], r[from] = r[from], r[to]
		}
		pathi++
		if pathi == len(path) {
			debug = true
		}
	}
	return r
}

func solve(s *state) int {
	minCost := math.MaxInt
	var winner *state
	stack := []*state{s}
	for prevLen := len(stack) - 1; len(stack) != 0; {
		if prevLen == len(stack) {
			panic("no progress")
		}
		ni := len(stack) - 1
		s := stack[ni].clone()
		stack = stack[0:ni]
		s.returnAmphs()
		if debug {
			fmt.Println(s)
			debug = false
		}
		if win, cost := s.hasWin(); win {
			if cost < minCost {
				winner = s
				minCost = cost
			}
			continue
		}
		for _, ns := range s.nextMoves() {
			stack = append(stack, ns)
		}
	}
	fmt.Println(winner)
	return minCost
}
