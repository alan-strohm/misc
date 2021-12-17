package day17

import (
	"fmt"
)

func check(err error) {
	if err != nil {
		panic(err)
	}
}

type point struct{ x, y int }
type target struct{ min, max point }

func getTarget(in string) *target {
	r := &target{}
	_, err := fmt.Sscanf(in, "target area: x=%d..%d, y=%d..%d", &r.min.x, &r.max.x, &r.min.y, &r.max.y)
	check(err)
	return r
}

func check(cond bool, format string, a ...interface{}) {
	if !cond {
		panic(fmt.Sprintf(format, a...))
	}
}

// Get the ith triangle number
func getTriangle(i int) int {
	return i * (i + 1) / 2
}

func part1(in string) int {
	t := getTarget(in)
	// We always return to 0,0 with velocity equal to the velocity we left with.
	// We want to return with 1 less velocity than it will take us to reach the
	// bottom edge of the target.
	check(t.min.y < 0, "expected negative y values, got %d", t.min.y)
	return getTriangle(-t.min.y - 1)
}
