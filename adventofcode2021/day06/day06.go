package day06

import (
	"bufio"
	"strconv"
	"strings"
)

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	scanner.Scan()
	seed := strings.Split(scanner.Text(), ",")
	total := len(seed)
	births := [80]int{}
	for _, f := range seed {
		n, err := strconv.Atoi(f)
		if err != nil {
			return 0, nil
		}
		births[n]++
	}
	for day := 0; day < len(births); day++ {
		total += births[day]
		next := day + 6 + 1
		if next < len(births) {
			births[next] += births[day]
		}
		next = day + 8 + 1
		if next < len(births) {
			births[next] += births[day]
		}
	}
	return total, nil
}
