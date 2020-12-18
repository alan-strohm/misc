package day15

type game struct {
	mem   map[int]int
	round int
}

func newGame() *game {
	return &game{mem: make(map[int]int)}
}

func (g *game) speak(num int) int {
	g.round++
	prev, ok := g.mem[num]
	g.mem[num] = g.round
	if !ok {
		return 0
	}
	return g.round - prev
}

func PlayGame(in []int, target int) int {
	g := newGame()
	next := 0
	for i := 0; i < target-1; i++ {
		if i < len(in) {
			next = g.speak(in[i])
		} else {
			next = g.speak(next)
		}
	}
	return next
}
