package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

func run(code string) error {
	return nil
}

func runFile(fname string) error {
	f, err := os.Open(fname)
	if err != nil {
		return err
	}
	b, err := io.ReadAll(f)
	if err != nil {
		return err
	}
	return run(string(b))
}

func runPrompt() error {
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}
		if err := run(scanner.Text()); err != nil {
			fmt.Printf("error: %s\n", err)
		}
	}
	return scanner.Err()
}

func main() {
	var err error
	if len(os.Args) > 2 {
		fmt.Println("Usage: lox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		err = runFile(os.Args[1])
	} else {
		err = runPrompt()
	}
	if err != nil {
		fmt.Printf("error: %s\n", err)
		os.Exit(1)
	}
}
