package io

import (
	"bufio"
	"fmt"
	"os"
)

type Scanner interface {
	ScanLine(l string) (done bool, err error)
}

func Scan(s Scanner, fname string) error {
	f, err := os.Open(fname)
	if err != nil {
		return fmt.Errorf("opening %s: %s", fname, err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if done, err := s.ScanLine(scanner.Text()); err != nil || done {
			return err
		}
	}
	return nil
}
