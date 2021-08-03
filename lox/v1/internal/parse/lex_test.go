package parse

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

func TestLex(t *testing.T) {
	cases := []struct {
		in  string
		exp []token.Token
	}{
		{"", []token.Token{{token.EOF, 0, ""}}},
		{"\n", []token.Token{{token.EOF, 1, ""}}},
		{" \n\r\t", []token.Token{{token.EOF, 4, ""}}},
		{"(){};,+-*!===<=>=!=<>/.", []token.Token{
			{token.LPAREN, 0, "("},
			{token.RPAREN, 1, ")"},
			{token.LBRACE, 2, "{"},
			{token.RBRACE, 3, "}"},
			{token.SEMICOLON, 4, ";"},
			{token.COMMA, 5, ","},
			{token.ADD, 6, "+"},
			{token.SUB, 7, "-"},
			{token.MUL, 8, "*"},
			{token.NEQ, 9, "!="},
			{token.EQL, 11, "=="},
			{token.LEQ, 13, "<="},
			{token.GEQ, 15, ">="},
			{token.NEQ, 17, "!="},
			{token.LSS, 19, "<"},
			{token.GTR, 20, ">"},
			{token.QUO, 21, "/"},
			{token.PERIOD, 22, "."},
			{token.EOF, 23, ""}}},
		{"123", []token.Token{
			{token.NUMBER, 0, "123"},
			{token.EOF, 3, ""}}},
		{"123.456", []token.Token{
			{token.NUMBER, 0, "123.456"},
			{token.EOF, 7, ""}}},
		{".456", []token.Token{
			{token.PERIOD, 0, "."},
			{token.NUMBER, 1, "456"},
			{token.EOF, 4, ""}}},
		{"123.", []token.Token{
			{token.ILLEGAL, 0, "number with trailing ."},
			{token.EOF, 4, ""}}},
		{`""`, []token.Token{
			{token.STRING, 0, `""`},
			{token.EOF, 2, ""}}},
		{`"string"`, []token.Token{
			{token.STRING, 0, `"string"`},
			{token.EOF, 8, ""}}},
		{"//foo\n123", []token.Token{
			{token.NUMBER, 6, "123"},
			{token.EOF, 9, ""}}},
		{"andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", []token.Token{
			{token.IDENT, 0, "andy"},
			{token.IDENT, 5, "formless"},
			{token.IDENT, 14, "fo"},
			{token.IDENT, 17, "_"},
			{token.IDENT, 19, "_123"},
			{token.IDENT, 24, "_abc"},
			{token.IDENT, 29, "ab123"},
			{token.IDENT, 35, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"},
			{token.EOF, 35 + 26*2 + 10 + 1, ""}}},
		{"and class else false for fun if nil or return super this true var while", []token.Token{
			{token.AND, 0, "and"},
			{token.CLASS, 4, "class"},
			{token.ELSE, 10, "else"},
			{token.FALSE, 15, "false"},
			{token.FOR, 21, "for"},
			{token.FUN, 25, "fun"},
			{token.IF, 29, "if"},
			{token.NIL, 32, "nil"},
			{token.OR, 36, "or"},
			{token.RETURN, 39, "return"},
			{token.SUPER, 46, "super"},
			{token.THIS, 52, "this"},
			{token.TRUE, 57, "true"},
			{token.VAR, 62, "var"},
			{token.WHILE, 66, "while"},
			{token.EOF, 71, ""}}},
	}
	for _, tc := range cases {
		l := lex(tc.in)
		out := make([]token.Token, 0, len(tc.exp))
		for l.scan() {
			out = append(out, l.token())
		}
		if len(out) != len(tc.exp) {
			t.Errorf("len(lex(%s)) = %d, want %d", tc.in, len(out), len(tc.exp))
		}
		for i, outToken := range out {
			outStr := fmt.Sprintf("%#v", outToken)
			expStr := fmt.Sprintf("%#v", tc.exp[i])
			if outStr != expStr {
				t.Errorf("lex(%s)[%d] =\n%s; want\n%s", tc.in, i, outStr, expStr)
			}
		}
	}
}
