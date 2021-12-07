package day07

import (
	"bufio"
	"math"
	"sort"
	"strconv"
	"strings"
)

func sum(in []float64, x float64) float64 {
	r := 0.0
	for _, i := range in {
		r += math.Abs(x - i)
	}
	return r
}

func p2sum(in []float64, x float64) float64 {
	r := 0.0
	for _, i := range in {
		d := math.Abs(x - i)
		r += d * (d + 1) / 2
	}
	return r
}

func solve(in []float64) float64 {
	i := len(in) / 2
	best := p2sum(in, in[i])
	for _, inc := range []float64{1, -1} {
		j := in[i] + inc
		for next := p2sum(in, j); next < best; {
			best = next
			j += inc
			next = p2sum(in, j)
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
	if p1 {
		return int(sum(pos, pos[len(pos)/2])), nil
	}
	return int(solve(pos)), nil
}
