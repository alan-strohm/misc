package day19

import (
	"flag"
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"../io"
)

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func dbg(in string) {
	if *dbgFlag {
		fmt.Println(in)
	}
}

type tokenType string
type exprType string

const (
	unknownToken tokenType = "ut"
	literalToken           = "l"
	refToken               = "r"
	altToken               = "|"
	eofToken               = "x"
)

type token struct {
	t       tokenType
	literal string
	ref     int
}

type rule struct {
	tokens []*token
	regex  string
	id     int
	inProg bool
	loop   bool
}

func (r *rule) done() {
	r.inProg = false
}

func (r *rule) toRegex(allRules map[int]*rule) (string, error) {
	if r.inProg {
		r.loop = true
		return "", nil
	}
	r.inProg = true
	defer r.done()

	if r.regex != "" {
		dbg(fmt.Sprintf("%d: returning already generated regex: %s", r.id, r.regex))
		return r.regex, nil
	}
	if len(r.tokens) == 1 && r.tokens[0].t == literalToken {
		r.regex = r.tokens[0].literal
		dbg(fmt.Sprintf("%d: returning new literal: %s", r.id, r.regex))
		return r.regex, nil
	}
	alt := false
	parts := make([]string, 0, len(r.tokens))
	for _, t := range r.tokens {
		switch t.t {
		case refToken:
			regex, err := ruleToRegex(allRules, t.ref)
			if err != nil {
				return "", err
			}
			if r.loop {
				return "", fmt.Errorf("loop from %d through %d", r.id, t.ref)
			}
			parts = append(parts, regex)
		case altToken:
			parts = append(parts, altToken)
			alt = true
		default:
			return "", fmt.Errorf("unexpected token: %s", t.t)
		}
	}
	r.regex = strings.Join(parts, "")
	if alt {
		r.regex = "(" + r.regex + ")"
	}
	return r.regex, nil
}

type matcher interface {
	CompileRules(rules map[int]*rule) error
	Matches(in string) bool
}

func ruleToRegex(allRules map[int]*rule, ref int) (string, error) {
	r, ok := allRules[ref]
	if !ok {
		return "", fmt.Errorf("could not find rule %d", ref)
	}
	return r.toRegex(allRules)
}

type regexMatcher struct {
	root *regexp.Regexp
}

func (m *regexMatcher) Matches(in string) bool {
	return m.root.MatchString(in)
}

func (m *regexMatcher) CompileRules(rules map[int]*rule) error {
	str, err := ruleToRegex(rules, 0)
	if err != nil {
		return err
	}
	dbg(fmt.Sprintf("compiled regex: %s", str))
	r, err := regexp.Compile("^" + str + "$")
	if err != nil {
		return err
	}
	m.root = r
	return nil
}

type part2Matcher struct {
	part1, part2 *regexp.Regexp
}

func (m *part2Matcher) Matches(in string) bool {
	numMatches := make([]int, 2)
	for i, re := range []*regexp.Regexp{m.part1, m.part2} {
		for prefix := re.FindString(in); prefix != "" && in != ""; prefix = re.FindString(in) {
			in = strings.TrimPrefix(in, prefix)
			numMatches[i]++
		}
	}
	return in == "" && numMatches[1] < numMatches[0] && numMatches[1] > 0 && numMatches[0] > 0
}

func (m *part2Matcher) CompileRules(rules map[int]*rule) error {
	res := make([]*regexp.Regexp, 2)
	for i, id := range []int{42, 31} {
		str, err := ruleToRegex(rules, id)
		if err != nil {
			return err
		}
		re, err := regexp.Compile("^" + str)
		if err != nil {
			return err
		}
		res[i] = re
	}
	m.part1, m.part2 = res[0], res[1]
	return nil
}

func lexLine(l string) (int, []*token, error) {
	parts := strings.Split(l, ": ")
	if len(parts) != 2 {
		return -1, nil, fmt.Errorf("malformed rule: %s", l)
	}
	id, err := strconv.Atoi(parts[0])
	if err != nil {
		return -1, nil, fmt.Errorf("could not parse id: %s", err)
	}
	tokens := strings.Split(parts[1], " ")
	r := make([]*token, len(tokens))
	if len(r) == 1 && []rune(tokens[0])[0] == '"' {
		t := []rune(tokens[0])
		r[0] = &token{t: literalToken, literal: string(t[1 : len(t)-1])}
		return id, r, nil
	}
	for i, t := range tokens {
		if t == "|" {
			r[i] = &token{t: altToken}
		} else {
			ref, err := strconv.Atoi(t)
			if err != nil {
				return -1, nil, fmt.Errorf("couldn't parse reference: %s", err)
			}
			r[i] = &token{t: refToken, ref: ref}
		}
	}
	return id, r, nil
}

type scanner struct {
	numBlanks  int
	rules      map[int]*rule
	m          matcher
	numMatched int
}

func (s *scanner) ScanLine(l string) (bool, error) {
	if l == "" {
		s.numBlanks++
		if s.numBlanks == 1 {
			if err := s.m.CompileRules(s.rules); err != nil {
				return false, err
			}
		}
		return false, nil
	}
	switch s.numBlanks {
	case 0:
		id, tokens, err := lexLine(l)
		if err != nil {
			return false, err
		}
		s.rules[id] = &rule{
			id:     id,
			tokens: tokens,
		}
	case 1:
		matches := s.m.Matches(l)
		if matches {
			dbg(fmt.Sprintf("match: %s", l))
		} else {
			dbg(fmt.Sprintf("no match: %s", l))
		}
		if matches {
			s.numMatched++
		}
	}
	return false, nil
}

func NumMatching(fname string) (int, error) {
	s := &scanner{rules: make(map[int]*rule), m: &regexMatcher{}}
	if err := io.Scan(s, fname); err != nil {
		return 0, err
	}
	return s.numMatched, nil
}

func NumMatchingPart2(fname string) (int, error) {
	s := &scanner{rules: make(map[int]*rule), m: &part2Matcher{}}
	if err := io.Scan(s, fname); err != nil {
		return 0, err
	}
	return s.numMatched, nil
}
