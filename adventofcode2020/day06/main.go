package main

import (
	"bufio"
	"fmt"
	"os"
)

type group interface {
	Reset()
	AddLine(l string)
	NumSelected() int
}

type orGroup map[rune]bool

func (g *orGroup) Reset() {
	*g = orGroup{}
}

func (g *orGroup) AddLine(l string) {
	for _, r := range []rune(l) {
		(*g)[r] = true
	}
}

func (g *orGroup) NumSelected() int {
	return len(*g)
}

type andGroup map[rune]bool

func (g *andGroup) Reset() {
	*g = nil
}

func (g *andGroup) AddLine(l string) {
	n := make(map[rune]bool)
	for _, r := range []rune(l) {
		n[r] = true
	}
	if *g == nil {
		*g = n
		return
	}
	for k, _ := range *g {
		if !n[k] {
			delete(*g, k)
		}
	}
}

func (g *andGroup) NumSelected() int {
	return len(*g)
}

func countSelected(g group) error {
	f, err := os.Open("./input.txt")
	if err != nil {
		return fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	totalSelected := 0
	g.Reset()
	for {
		done := !scanner.Scan()
		if done || scanner.Text() == "" {
			totalSelected += g.NumSelected()
			g.Reset()
		} else {
			g.AddLine(scanner.Text())
		}
		if done {
			break
		}
	}
	fmt.Printf("Total selected: %d\n", totalSelected)
	return nil
}

func main() {
	if err := countSelected(&orGroup{}); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	var g andGroup
	if err := countSelected(&g); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
