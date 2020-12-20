package day18

import (
	"errors"
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/timtadh/lexmachine"
	"github.com/timtadh/lexmachine/machines"

	"../io"
)

const lowestPrecedence = 0

type precMap map[int]int

var (
	literals = []string{
		"(", ")", "+", "*",
	}
	tokens   = []string{"NUM", "EOL"}
	tokenIDs = make(map[string]int)
	lexer    *lexmachine.Lexer

	eqPrecedence  precMap
	revPrecedence precMap
)

func init() {
	initTokens()
	eqPrecedence = precMap{
		tokenIDs["*"]: 1,
		tokenIDs["+"]: 1,
	}
	revPrecedence = precMap{
		tokenIDs["*"]: 1,
		tokenIDs["+"]: 2,
	}

	var err error
	lexer, err = initLexer()
	if err != nil {
		panic(err)
	}
}

func initTokens() {
	tokens = append(tokens, literals...)
	for i, tok := range tokens {
		tokenIDs[tok] = i
	}
}

func token(name string) lexmachine.Action {
	return func(s *lexmachine.Scanner, m *machines.Match) (interface{}, error) {
		return s.Token(tokenIDs[name], string(m.Bytes), m), nil
	}
}

func skip() lexmachine.Action {
	return func(s *lexmachine.Scanner, m *machines.Match) (interface{}, error) {
		return nil, nil
	}
}

func initLexer() (*lexmachine.Lexer, error) {
	lexer := lexmachine.NewLexer()

	for _, lit := range literals {
		r := "\\" + strings.Join(strings.Split(lit, ""), "\\")
		lexer.Add([]byte(r), token(lit))
	}
	lexer.Add([]byte(`[0-9]+`), token("NUM"))
	lexer.Add([]byte(`\n`), token("EOL"))
	lexer.Add([]byte(`( |\t)`), skip())
	return lexer, lexer.Compile()
}

type expType string

const (
	unknownExp expType = ""
	literalExp         = "l"
	binExp             = "b"
	groupExp           = ""
)

type expr struct {
	t     expType
	op    int
	num   int
	x, y  *expr
	group *expr
}

type parser struct {
	tok *lexmachine.Token
	eof bool

	prec precMap

	s *lexmachine.Scanner
}

func newParser(text []byte, prec precMap) (*parser, error) {
	s, err := lexer.Scanner(text)
	if err != nil {
		return nil, err
	}
	return &parser{s: s, prec: prec}, nil
}

func (p *parser) Parse() (*expr, error) {
	fmt.Printf("prec: %v\n", p.prec)
	if err := p.next(); err != nil {
		return nil, err
	}
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.tok.Type != tokenIDs["EOL"] {
		return nil, errors.New("unexpected text")
	}
	return expr, nil
}

func (p *parser) next() error {
	tok, err, eof := p.s.Next()
	if err != nil {
		return err
	}
	p.eof = eof
	if p.eof {
		p.tok = &lexmachine.Token{Type: tokenIDs["EOL"]}
		return nil
	}
	p.tok = tok.(*lexmachine.Token)
	return nil
}

func (p *parser) expression() (*expr, error) {
	return p.binaryExpression(lowestPrecedence)
}

func (p *parser) binaryExpression(prec int) (*expr, error) {
	e, err := p.primary()
	if err != nil {
		return nil, err
	}
	for opPrec, ok := p.prec[p.tok.Type]; ok; opPrec, ok = p.prec[p.tok.Type] {
		if opPrec < prec {
			return e, nil
		}
		op := p.tok.Type
		if err := p.next(); err != nil {
			return nil, err
		}
		y, err := p.binaryExpression(opPrec + 1)
		if err != nil {
			return nil, err
		}
		e = &expr{x: e, t: binExp, op: op, y: y}
	}
	return e, nil
}

func (p *parser) primary() (*expr, error) {
	switch p.tok.Type {
	case tokenIDs["NUM"]:
		return p.num(string(p.tok.Lexeme))
	case tokenIDs["("]:
		return p.group()
	}
	return nil, errors.New("expected an expression")
}

func (p *parser) num(lexeme string) (*expr, error) {
	n, err := strconv.Atoi(lexeme)
	if err != nil {
		return nil, err
	}
	return &expr{t: literalExp, num: n}, p.next()
}

func (p *parser) group() (*expr, error) {
	if err := p.next(); err != nil {
		return nil, err
	}
	e, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.tok.Type != tokenIDs[")"] {
		return nil, errors.New("unmatched paren")
	}
	return &expr{t: groupExp, group: e}, p.next()
}

func evalBin(e *expr) (int, error) {
	if e == nil || e.x == nil || e.y == nil {
		log.Fatalf("internal error: invalid call to evalBinOp(%v)", e)
	}
	x, err := eval(e.x)
	if err != nil {
		return 0, err
	}
	y, err := eval(e.y)
	if err != nil {
		return 0, err
	}
	switch e.op {
	case tokenIDs["*"]:
		return x * y, nil
	case tokenIDs["+"]:
		return x + y, nil
	}
	return 0, fmt.Errorf("unknown binary operator: %s", tokens[e.op])
}

func eval(e *expr) (int, error) {
	if e == nil {
		log.Fatal("nil expr")
	}
	switch e.t {
	case literalExp:
		return e.num, nil
	case binExp:
		return evalBin(e)
	case groupExp:
		return eval(e.group)
	}
	return 0, fmt.Errorf("unknown expr: %v", e)
}

func EvalStringExpr(s string, prec precMap) (int, error) {
	p, err := newParser([]byte(s), prec)
	if err != nil {
		return 0, err
	}
	expr, err := p.Parse()
	if err != nil {
		return 0, err
	}
	return eval(expr)
}

type scanner struct {
	prec precMap
	sum  int
}

func (s *scanner) ScanLine(l string) (bool, error) {
	i, err := EvalStringExpr(l, s.prec)
	if err != nil {
		return false, err
	}
	s.sum += i
	return false, nil
}

func SumFile(fname string, prec precMap) (int, error) {
	s := scanner{prec: prec}
	if err := io.Scan(&s, fname); err != nil {
		return 0, err
	}
	return s.sum, nil
}
