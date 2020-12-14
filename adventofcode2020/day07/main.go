package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type childRef struct {
	child        *node
	numContained int
}

type node struct {
	color    string
	parents  map[string]*node
	children map[string]*childRef
}

type rules map[string]*node

var ruleRe = regexp.MustCompile(`^(.*) bags contain (.*).$`)

func (r *rules) getNode(color string) *node {
	n, ok := (*r)[color]
	if !ok {
		n = &node{
			color:    color,
			parents:  make(map[string]*node),
			children: make(map[string]*childRef)}
		(*r)[color] = n
	}
	return n
}

func (r *rules) addNodeLine(l string) error {
	matches := ruleRe.FindStringSubmatch(l)
	if len(matches) != 3 {
		return fmt.Errorf("malformed line")
	}
	n := r.getNode(matches[1])
	if matches[2] == "no other bags" {
		return nil
	}
	children := strings.Split(matches[2], ", ")
	for _, v := range children {
		v = strings.TrimSuffix(strings.TrimSuffix(v, " bag"), " bags")
		numStr := strings.Fields(v)[0]
		color := strings.TrimSpace(strings.TrimPrefix(v, numStr))
		num, err := strconv.Atoi(numStr)
		if err != nil {
			return fmt.Errorf("could not parse number of %s bags allowed in %s bags", color, n.color)
		}
		cn := r.getNode(color)
		n.children[color] = &childRef{
			child:        cn,
			numContained: num,
		}
		cn.parents[n.color] = n
	}
	return nil
}

func read() (*rules, error) {
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	r := &rules{}
	for scanner.Scan() {
		if err := r.addNodeLine(scanner.Text()); err != nil {
			return nil, err
		}
	}
	return r, nil
}

func numContainers(n *node, seen map[string]bool) int {
	r := 0
	for _, p := range n.parents {
		if _, ok := seen[p.color]; !ok {
			seen[p.color] = true
			r += numContainers(p, seen) + 1
		}
	}
	return r
}

func numContained(n *node) int {
	r := 0
	for _, c := range n.children {
		r += c.numContained
		r += c.numContained * numContained(c.child)
	}
	return r
}

func part1(r *rules) error {
	start := (*r)["shiny gold"]
	if start == nil {
		return errors.New("could not find start")
	}
	fmt.Printf("%d possible containers\n", numContainers(start, make(map[string]bool)))
	return nil
}

func part2(r *rules) error {
	start := (*r)["shiny gold"]
	if start == nil {
		return errors.New("could not find start")
	}
	fmt.Printf("%d contained\n", numContained(start))
	return nil
}

func main() {
	r, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part1(r); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := part2(r); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
