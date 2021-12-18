package day17

import (
	"fmt"
	"math"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type point struct{ x, y int }
type target struct{ min, max point }

func getTarget(in string) *target {
	r := &target{}
	if _, err := fmt.Sscanf(in, "target area: x=%d..%d, y=%d..%d", &r.min.x, &r.max.x, &r.min.y, &r.max.y); err != nil {
		panic(err)
	}
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

func ceil(num, den int) int {
	return int(math.Ceil(float64(num) / float64(den)))
}

func max(a, b int) int {
	if b > a {
		return b
	}
	return a
}

func part2(in string) int {
	t := getTarget(in)
	opts := map[point]bool{}
	stablevxs := []int{}         // vxs which are within the target for any future step.
	futurevys := map[int][]int{} // step -> vys which will be within the target on that step.
	for step := 1; ; step++ {
		prevTri := getTriangle(step - 1)
		vxs := []int{}
		// We can go vx*step - getTriangle(step-1) for vx <= step
		fromx, tox := t.min.x+prevTri, t.max.x+1+prevTri
		for vx := max(step, ceil(fromx, step)); vx < ceil(tox, step); vx++ {
			if vx == step {
				stablevxs = append(stablevxs, vx)
			} else {
				vxs = append(vxs, vx)
			}
		}
		vxs = append(vxs, stablevxs...)

		vys := []int{}
		// We can go vy*step + getTriangle(step-1) distance.
		fromy, toy := -t.max.y-prevTri, -t.min.y+1-prevTri
		for vy := max(0, ceil(fromy, step)); vy < ceil(toy, step); vy++ {
			vys = append(vys, -vy)
			// For each negative vy, a positive vy-1 is possible 2*(vy-1)+1 steps later
			future := step + 2*vy - 1
			if future > step && vy > 1 {
				futurevys[future] = append(futurevys[future], vy-1)
			}
		}
		vys = append(vys, futurevys[step]...)
		delete(futurevys, step)

		lib.Dbg("vxs: %v, vys: %v\n", vxs, vys)
		for _, vx := range vxs {
			for _, vy := range vys {
				opts[point{vx, vy}] = true
			}
		}
		lib.Dbg("step: %d, ceil(toy, step): %d, futurevys: %v\n", step, ceil(toy, step), futurevys)
		if ceil(toy, step) <= 1 && len(futurevys) == 0 {
			break
		}
	}
	for key, _ := range opts {
		lib.Dbg("%d,%d\n", key.x, key.y)
	}
	return len(opts)
}
