package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type opCode int

const (
	unknownOp opCode = iota
	accOp
	nopOp
	jmpOp
)

type instruction struct {
	op     opCode
	arg    int
	lineNo int
}

func parseOpCode(code string) (opCode, error) {
	switch code {
	case "acc":
		return accOp, nil
	case "nop":
		return nopOp, nil
	case "jmp":
		return jmpOp, nil
	default:
		return unknownOp, fmt.Errorf("unknown op: %s", code)
	}
}

func parse(l string) (*instruction, error) {
	fields := strings.Fields(l)
	if len(fields) != 2 {
		return nil, fmt.Errorf("invalid instruction: %s", l)
	}
	op, err := parseOpCode(fields[0])
	if err != nil {
		return nil, fmt.Errorf("invalid instruction: ", err)
	}
	arg, err := strconv.Atoi(fields[1])
	if err != nil {
		return nil, fmt.Errorf("invalid instruction: ", err)
	}
	return &instruction{op: op, arg: arg}, nil
}

func read() ([]*instruction, error) {
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	r := make([]*instruction, 0)
	for lineNo := 0; scanner.Scan(); lineNo++ {
		i, err := parse(scanner.Text())
		if err != nil {
			return nil, fmt.Errorf("could not parse line %d: %s", lineNo, err)
		}
		i.lineNo = lineNo
		r = append(r, i)
	}
	return r, nil
}

func run(in []*instruction) (int, bool) {
	acc := 0
	seen := make(map[int]bool)
	for pc := 0; pc < len(in); {
		if seen[pc] {
			return acc, true
		}
		seen[pc] = true
		switch in[pc].op {
		case jmpOp:
			pc += in[pc].arg
			continue
		case accOp:
			acc += in[pc].arg
			pc++
		case nopOp:
			pc++
		}
	}
	return acc, false
}

func part1(in []*instruction) {
	acc, _ := run(in)
	fmt.Printf("Part1 answer is %d\n", acc)
}

func toggleOp(i *instruction) bool {
	switch i.op {
	case jmpOp:
		i.op = nopOp
	case nopOp:
		i.op = jmpOp
	default:
		return false
	}
	return true
}

func part2(in []*instruction) {
	for _, i := range in {
		if !toggleOp(i) {
			continue
		}
		acc, loop := run(in)
		if !loop {
			fmt.Printf("Part2 answer is %d\n", acc)
			return
		}
		toggleOp(i)
	}
}

func main() {
	in, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	part1(in)
	part2(in)
}
