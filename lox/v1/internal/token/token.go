package token

import (
	"fmt"
	"strconv"
)

type Type int

const (
	ILLEGAL Type = iota // error occurred; value is text of error
	EOF

	IDENT
	STRING // string literal (includes quotes)
	NUMBER // number literal

	LPAREN
	LBRACE
	COMMA
	PERIOD

	RPAREN
	RBRACE
	SEMICOLON

	SUB
	ADD
	MUL
	QUO

	EQL
	LSS
	GTR
	ASSIGN
	NOT

	NEQ
	LEQ
	GEQ

	keyword_beg
	AND
	CLASS
	ELSE
	FALSE
	FUN

	FOR
	IF
	NIL
	OR
	PRINT

	RETURN
	SUPER
	THIS
	TRUE
	VAR
	WHILE
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	IDENT:  "IDENT",
	STRING: "STRING",
	NUMBER: "NUMBER",

	LPAREN: "(",
	LBRACE: ")",
	COMMA:  ",",
	PERIOD: ".",

	RPAREN:    ")",
	RBRACE:    "}",
	SEMICOLON: ":",

	SUB: "-",
	ADD: "+",
	MUL: "*",
	QUO: "/",

	EQL:    "==",
	LSS:    "<",
	GTR:    ">",
	ASSIGN: "=",
	NOT:    "!",

	NEQ: "!=",
	LEQ: "<=",
	GEQ: ">=",

	AND:   "and",
	CLASS: "class",
	ELSE:  "else",
	FALSE: "false",
	FUN:   "fun",

	FOR:   "for",
	IF:    "if",
	NIL:   "nil",
	OR:    "or",
	PRINT: "print",

	RETURN: "return",
	SUPER:  "super",
	THIS:   "this",
	TRUE:   "true",
	VAR:    "var",
	WHILE:  "while",
}

const (
	LowestPrec  = 0
	UnaryPrec   = 6
	HighestPrec = 7
)

func (t Type) Precedence() int {
	switch t {
	case EQL, NEQ, GTR, LSS, GEQ, LEQ:
		return 3
	case ADD, SUB:
		return 4
	case MUL, QUO:
		return 5
	}
	return LowestPrec
}

var keywords map[string]Type

func init() {
	keywords = make(map[string]Type)
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// Lookup maps an identifier to its keyword token type or IDENT (if not a keyword).
func Lookup(ident string) Type {
	if typ, is_keyword := keywords[ident]; is_keyword {
		return typ
	}
	return IDENT
}

// IsKeyword returns true for token types corresponding to keywords; it returns false otherwise.
func (t Type) IsKeyword() bool { return keyword_beg < t && t < keyword_end }

// String returns the string corresponding to the token tok.
// For operators, delimiters, and keywords the string is the actual
// token character sequence (e.g., for the token ADD, the string is
// "+"). For all other tokens the string corresponds to the token
// constant name (e.g. for the token IDENT, the string is "IDENT").
func (t Type) String() string {
	s := ""
	if 0 <= t && t < Type(len(tokens)) {
		s = tokens[t]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(t)) + ")"
	}
	return s
}

type Pos int

type Token struct {
	Type Type
	Pos  Pos
	Val  string
}

func (t Token) String() string {
	switch {
	case t.Type == EOF:
		return "EOF"
	case t.Type == ILLEGAL:
		return t.Val
	case t.Type.IsKeyword():
		return fmt.Sprintf("<%s>", t.Type)
	case len(t.Val) > 10:
		return fmt.Sprintf("%.10q...", t.Val)
	}
	return fmt.Sprintf("%q", t.Val)
}
