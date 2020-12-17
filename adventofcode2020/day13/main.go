package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"math/big"
	"os"
	"strconv"
	"strings"
)

var in = flag.String("in", "./input.txt", "File to read.")

func init() {
	flag.Parse()
}

func read() (now int, schedule []string, errOut error) {
	now, schedule, errOut = -1, nil, nil
	f, err := os.Open(*in)
	if err != nil {
		errOut = fmt.Errorf("opening %s: %s", *in, err)
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	if !scanner.Scan() {
		errOut = errors.New("missing timestamp")
		return
	}
	now, err = strconv.Atoi(scanner.Text())
	if err != nil {
		errOut = fmt.Errorf("invalid timestamp: %s", err)
		return
	}
	if !scanner.Scan() {
		errOut = errors.New("missing schedule")
		return
	}
	return now, strings.Split(scanner.Text(), ","), nil
}

func part1() error {
	now, schedule, err := read()
	if err != nil {
		return err
	}
	minLine, minWait := -1, -1
	for _, line := range schedule {
		if line == "x" {
			continue
		}
		lineNo, err := strconv.Atoi(line)
		if err != nil {
			return fmt.Errorf("invalid line: %s", line)
		}
		wait := lineNo - now%lineNo
		if minWait == -1 || wait < minWait {
			minWait = wait
			minLine = lineNo
		}
	}
	fmt.Printf("Part1: %d * %d = %d\n", minLine, minWait, minLine*minWait)
	return nil
}

func part2() error {
	_, schedule, err := read()
	if err != nil {
		return err
	}
	rems := make([]int, 0)
	groups := make([]*big.Int, 0)
	product := big.NewInt(1)
	for i, line := range schedule {
		if line == "x" {
			continue
		}
		g, err := strconv.Atoi(line)
		if err != nil {
			return fmt.Errorf("invalid line: %s", line)
		}
		rems = append(rems, (g-i)%g)
		group := big.NewInt(int64(g))
		product.Mul(product, group)
		groups = append(groups, group)
	}
	res := big.NewInt(0)
	for i, group := range groups {
		gcd := big.NewInt(0)
		y := big.NewInt(0).Div(product, group)
		x := big.NewInt(0)
		gcd.GCD(x, nil, y, group)
		term := x.Mod(x, group)
		term.Mul(term, y)
		term.Mul(term, big.NewInt(int64(rems[i])))
		res.Add(res, term)
	}
	res.Mod(res, product)
	fmt.Printf("Part2: %s\n", res)
	return nil
}

func main() {
	if err := part1(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part2(); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
