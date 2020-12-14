package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
)

func toBin(zero, one rune, s []rune) error {
	for i, r := range s {
		if r == zero {
			s[i] = '0'
		} else if r == one {
			s[i] = '1'
		} else {
			return fmt.Errorf("unknown rune: %s", s[i])
		}
	}
	return nil
}

func binToRowColumn(bin string) (int64, int64, error) {
	if len(bin) != 10 {
		return 0, 0, errors.New("string too long")
	}
	rowStr := []rune(bin)[0:7]
	colStr := []rune(bin)[7:10]
	if err := toBin('F', 'B', rowStr); err != nil {
		return 0, 0, fmt.Errorf("invalid row: %s", err)
	}
	if err := toBin('L', 'R', colStr); err != nil {
		return 0, 0, fmt.Errorf("invalid col: %s", err)
	}
	row, err := strconv.ParseInt(string(rowStr), 2, 8)
	if err != nil {
		return 0, 0, fmt.Errorf("invalid row: ", err)
	}
	col, err := strconv.ParseInt(string(colStr), 2, 8)
	if err != nil {
		return 0, 0, fmt.Errorf("invalid col: ", err)
	}
	return row, col, nil
}

func read() ([]bool, error) {
	f, err := os.Open("./input.txt")
	if err != nil {
		return nil, fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	ids := make([]bool, 1024)
	for scanner.Scan() {
		row, col, err := binToRowColumn(scanner.Text())
		if err != nil {
			return nil, err
		}
		id := row*8 + col
		ids[id] = true
	}
	return ids, nil
}

func main() {
	ids, err := read()
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}

	minSeat, maxSeat, mySeat := 0, 0, 0
	for i, occupied := range ids {
		if !occupied && minSeat == 0 {
			continue
		}
		if occupied && minSeat == 0 {
			minSeat = i
		} else if !occupied && mySeat == 0 {
			mySeat = i
		} else if !occupied {
			maxSeat = i
			break
		}
	}
	fmt.Printf("Last occupied seat: %d\n", maxSeat)
	fmt.Printf("My seat: %d\n", mySeat)
}
