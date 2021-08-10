package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/alan-strohm/misc/lox/v1/internal/dot"
	"github.com/alan-strohm/misc/lox/v1/internal/interpret"
	"github.com/alan-strohm/misc/lox/v1/internal/parse"
)

var dotF = flag.String("dotFile", "", "if non-empty, file to write parse tree in dot format.")

var interpreter = interpret.New()

func run(code string) error {
	p, err := parse.ParseProgram(code)
	if err != nil {
		return fmt.Errorf("parsing error: %s", err)
	}
	if *dotF != "" {
		out := dot.ProgToDot(p)
		os.WriteFile(*dotF, []byte(out), 0666)
	}
	if err := interpreter.Interpret(p); err != nil {
		return fmt.Errorf("runtime error: %s", err)
	}
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
			fmt.Println(err)
		}
	}
	return scanner.Err()
}

func main() {
	flag.Parse()
	var err error
	if len(flag.Args()) > 1 {
		fmt.Println("Usage: lox [script]")
		flag.PrintDefaults()
		os.Exit(64)
	} else if len(flag.Args()) == 1 {
		err = runFile(flag.Arg(0))
	} else {
		err = runPrompt()
	}
	if err != nil {
		fmt.Printf("error: %s\n", err)
		os.Exit(1)
	}
}
