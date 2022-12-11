package day09

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type pos [2]int

func (p pos) sub(o pos) pos { return [2]int{p[0] - o[0], p[1] - o[1]} }
func (p pos) add(o pos) pos { return [2]int{p[0] + o[0], p[1] + o[1]} }

func newTail(h, t pos) pos {
	diff := h.sub(t)
	move := false
	for i, dim := range diff {
		abs := math.Abs(float64(dim))
		if abs > 1 {
			diff[i] = dim / int(abs)
			move = true
		}
	}
	if move {
		return t.add(diff)
	}
	return t
}

type cmd struct {
	dir   pos // unit vector
	count int
}

func must(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func parseCmd(l string) cmd {
	var dir byte
	var count int
	_, err := fmt.Sscanf(l, "%c %d", &dir, &count)
	must(err)

	dirs := map[byte][2]int{
		'R': [2]int{1, 0}, 'L': [2]int{-1, 0}, 'U': [2]int{0, 1}, 'D': [2]int{0, -1},
	}
	return cmd{dir: dirs[dir], count: count}
}

func parseCmds(s *bufio.Scanner) (r []cmd) {
	for s.Scan() {
		r = append(r, parseCmd(s.Text()))
	}
	return
}

func countVisited(length int, cmds []cmd) int {
	rope := make([]pos, length)
	visited := map[[2]int]bool{rope[len(rope)-1]: true}
	for _, c := range cmds {
		for i := 0; i < c.count; i++ {
			rope[0] = rope[0].add(c.dir)
			for i := 1; i < len(rope); i++ {
				rope[i] = newTail(rope[i-1], rope[i])
			}
			visited[rope[len(rope)-1]] = true
		}
	}
	return len(visited)
}

func Run(s *bufio.Scanner, isPart1 bool) (int, error) {
	cmds := parseCmds(s)
	if isPart1 {
		return countVisited(2, cmds), nil
		return 0, nil
	}
	return countVisited(10, cmds), nil
}
