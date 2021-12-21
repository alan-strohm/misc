package day21

import "fmt"

func newDetDie() func() int {
	i := 1
	return func() int {
		r := (3*i + 3) % 10
		i += 3
		return r
	}
}

// The state of the game.
type state struct {
	pos   [2]int
	score [2]int
}

func initState(pos [2]int) state {
	for i, _ := range pos {
		pos[i]--
	}
	return state{pos: pos}
}

// Advance player by amt and return the resulting state.
func (s state) adv(player, amt int) state {
	r := s
	r.pos[player] += amt
	r.pos[player] %= 10
	r.score[player] += r.pos[player] + 1
	return r
}

func part1(pos [2]int) int {
	next := newDetDie()
	st := initState(pos)
	cur := 1
	numRolls := 0
	for ; st.score[cur] < 1000; numRolls += 3 {
		cur ^= 1
		roll := next()
		st = st.adv(cur, roll)
		fmt.Printf("Player %d rolls  %d and moves to space %d for a total score of %d\n", cur+1, roll, st.pos[cur]+1, st.score[cur])
	}
	fmt.Printf("Player %d loses with a score of %d after %d rolls\n", cur^1, st.score[cur^1], numRolls)
	return st.score[cur^1] * numRolls
}

// The number of dice roles that result in each possible turn outcome.
var turnCombos [10]int

func init() {
	sides := []int{1, 2, 3}
	for _, r1 := range sides {
		for _, r2 := range sides {
			for _, r3 := range sides {
				turnCombos[(r1+r2+r3)%10]++
			}
		}
	}
	fmt.Println(turnCombos)
}

func max(a, b int) int {
	if b > a {
		return b
	}
	return a
}

func part2(pos [2]int) int {
	// The number of universes in each state we are tracking.
	unis := map[state]int{
		initState(pos): 1,
	}
	wins := [2]int{}
	turns := 0
	for cur := 0; len(unis) > 0; cur ^= 1 {
		nextUnis := map[state]int{}
		numUnis := 0
		for st, prev := range unis {
			for adv, inc := range turnCombos {
				if inc == 0 {
					continue
				}
				next := st.adv(cur, adv)
				if next.score[cur] >= 21 {
					wins[cur] += prev * inc
				} else {
					nextUnis[next] += prev * inc
					numUnis += prev * inc
				}
			}
		}
		unis = nextUnis
		turns++
		fmt.Printf("after turn %d (player %d), %d universes\n", turns, cur+1, numUnis)
	}
	return max(wins[0], wins[1])
}
