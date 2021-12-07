package day07

import (
	"bufio"
	"math"
	"sort"
	"strconv"
	"strings"
)

func sum(in []float64, x float64) float64 {
	r := float64(0)
	for _, i := range in {
		r += math.Abs(x - i)
	}
	return r
}

func solve(in []float64) float64 {
	i := len(in) / 2
	best := sum(in, in[i])
	for _, inc := range []int{1, -1} {
		j := i + inc
		for next := sum(in, in[j]); next < best; {
			best = next
			j += inc
			next = sum(in, in[j])
		}
	}
	return best
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	scanner.Scan()
	in := strings.Split(scanner.Text(), ",")
	pos := make([]float64, len(in))
	for i, s := range in {
		n, err := strconv.Atoi(s)
		if err != nil {
			return 0, nil
		}
		pos[i] = float64(n)
	}
	sort.Float64s(pos)
	return int(solve(pos)), nil
}
