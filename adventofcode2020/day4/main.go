package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type ID map[string]string

func (id *ID) addLine(l string) error {
	for _, field := range strings.Fields(l) {
		parts := strings.Split(field, ":")
		if len(parts) != 2 {
			return fmt.Errorf("invalid field [%s]", field)
		}
		(*id)[parts[0]] = parts[1]
	}
	return nil
}

func (id *ID) isValidPart1() bool {
	reqFields := []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
	for _, field := range reqFields {
		if _, ok := (*id)[field]; !ok {
			return false
		}
	}
	return true
}

var hclRe = regexp.MustCompile(`^#[0-9a-f]{6}$`)
var eclRe = regexp.MustCompile(`^(amb|blu|brn|gry|grn|hzl|oth)$`)
var pidRe = regexp.MustCompile(`^[0-9]{9}$`)

func (id *ID) isValidPart2() bool {
	if !id.isValidPart1() {
		return false
	}
	byr, err := strconv.Atoi((*id)["byr"])
	if err != nil || byr > 2002 || byr < 1920 || len((*id)["byr"]) != 4 {
		return false
	}
	iyr, err := strconv.Atoi((*id)["iyr"])
	if err != nil || iyr > 2020 || iyr < 2010 || len((*id)["iyr"]) != 4 {
		return false
	}
	eyr, err := strconv.Atoi((*id)["eyr"])
	if err != nil || eyr > 2030 || eyr < 2020 || len((*id)["eyr"]) != 4 {
		return false
	}

	hgtStr := (*id)["hgt"]
	hgt, err := strconv.Atoi(string([]rune(hgtStr)[0 : len(hgtStr)-2]))
	if err != nil {
		return false
	}
	if strings.HasSuffix(hgtStr, "cm") {
		if hgt < 150 || hgt > 193 {
			return false
		}
	} else if strings.HasSuffix(hgtStr, "in") {
		if hgt < 59 || hgt > 76 {
			return false
		}
	} else {
		return false
	}

	if !hclRe.MatchString((*id)["hcl"]) {
		return false
	}

	if !eclRe.MatchString((*id)["ecl"]) {
		return false
	}

	if !pidRe.MatchString((*id)["pid"]) {
		return false
	}
	return true
}

func countValid(part2 bool) error {
	f, err := os.Open("./input.txt")
	if err != nil {
		return fmt.Errorf("opening input.txt: %s", err)
	}
	scanner := bufio.NewScanner(f)
	numValid := 0
	currentID := &ID{}
	for scanner.Scan() {
		if scanner.Text() == "" {
			if !part2 && currentID.isValidPart1() {
				numValid++
			}
			if part2 && currentID.isValidPart2() {
				numValid++
			}
			currentID = &ID{}
		} else {
			currentID.addLine(scanner.Text())
		}
	}
	fmt.Printf("%d valid IDs\n", numValid)
	return nil
}

func main() {
	if err := countValid(false); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	if err := countValid(true); err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
}
