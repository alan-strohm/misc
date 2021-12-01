package day02

import (
	"bufio"
	"fmt"
	"strconv"
	"strings"
)

type sub struct {
	depth, pos, aim int
	part1           bool
}

func (s *sub) do(cmd string, arg int) {
	if s.part1 {
		s.p1(cmd, arg)
	} else {
		s.p2(cmd, arg)
	}
}

func (s *sub) p1(cmd string, arg int) {
	switch cmd {
	case "forward":
		s.pos += arg
	case "down":
		s.depth += arg
	case "up":
		s.depth -= arg
	}
}

func (s *sub) p2(cmd string, arg int) {
	switch cmd {
	case "forward":
		s.pos += arg
		s.depth += arg * s.aim
	case "down":
		s.aim += arg
	case "up":
		s.aim -= arg
	}
}

func Run(scanner *bufio.Scanner, part1 bool) (int, error) {
	s := sub{part1: part1}
	for i := 0; scanner.Scan(); i++ {
		parts := strings.Split(scanner.Text(), " ")
		arg, err := strconv.Atoi(parts[1])
		if err != nil {
			return 0, fmt.Errorf("invalid line [%d]: %s", i, scanner.Text())
		}
		s.do(parts[0], arg)
	}
	return s.pos * s.depth, nil
}
