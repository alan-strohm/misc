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

type Entry struct {
	password   string
	char       rune
	num1, num2 int
}

var re = regexp.MustCompile(`(\d+)-(\d+) (.): (.*)`)

func parseEntry(l string) (*Entry, error) {
	matches := re.FindStringSubmatch(l)
	if len(matches) != 5 {
		return nil, errors.New("unexpected format")
	}
	e := &Entry{}
	var err error
	if e.num1, err = strconv.Atoi(matches[1]); err != nil {
		return nil, fmt.Errorf("num1 %s is not a number", matches[1])
	}
	if e.num2, err = strconv.Atoi(matches[2]); err != nil {
		return nil, fmt.Errorf("num2 %s is not a number", matches[1])
	}
	runes := []rune(matches[3])
	if len(runes) != 1 {
		return nil, fmt.Errorf("expected single rune in %s, got %d", matches[3], len(runes))
	}
	e.char = runes[0]
	e.password = matches[4]
	return e, nil
}

func read() ([]*Entry, error) {
	r := make([]*Entry, 0)
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	for i := 0; scanner.Scan(); i++ {
		e, err := parseEntry(scanner.Text())
		if err != nil {
			return nil, fmt.Errorf("couldn't parse line %d: %s", i, err)
		}
		r = append(r, e)
	}
	return r, nil
}

func part1(in []*Entry) {
	numValid := 0
	for _, e := range in {
		n := strings.Count(e.password, string(e.char))
		if n >= e.num1 && n <= e.num2 {
			numValid++
		}
	}
	fmt.Printf("%d passwords are valid\n", numValid)
}

func part2(in []*Entry) error {
	numValid := 0
	for i, e := range in {
		runes := []rune(e.password)
		if e.num1-1 > len(runes) || e.num2-1 > len(runes) {
			return fmt.Errorf("oob index on entry %d", i)
		}
		if (runes[e.num1-1] == e.char) != (runes[e.num2-1] == e.char) {
			numValid++
		}
	}
	fmt.Printf("%d passwords are valid\n", numValid)
	return nil
}

func main() {
	in, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	part1(in)
	if err := part2(in); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
