package parse

import (
	"fmt"
	"testing"
)

func TestLex(t *testing.T) {
	cases := []struct {
		in  string
		exp []Item
	}{
		{"", []Item{{ItemEOF, 0, "", 1}}},
		{"\n", []Item{{ItemEOF, 1, "", 2}}},
		{" \n\r\t", []Item{{ItemEOF, 4, "", 2}}},
		{"(){};,+-*!===<=>=!=<>/.", []Item{
			{ItemLeftParen, 0, "(", 1},
			{ItemRightParen, 1, ")", 1},
			{ItemLeftBrace, 2, "{", 1},
			{ItemRightBrace, 3, "}", 1},
			{ItemSemicolon, 4, ";", 1},
			{ItemComma, 5, ",", 1},
			{ItemPlus, 6, "+", 1},
			{ItemMinus, 7, "-", 1},
			{ItemStar, 8, "*", 1},
			{ItemBangEqual, 9, "!=", 1},
			{ItemEqualEqual, 11, "==", 1},
			{ItemLessEqual, 13, "<=", 1},
			{ItemGreaterEqual, 15, ">=", 1},
			{ItemBangEqual, 17, "!=", 1},
			{ItemLess, 19, "<", 1},
			{ItemGreater, 20, ">", 1},
			{ItemSlash, 21, "/", 1},
			{ItemDot, 22, ".", 1},
			{ItemEOF, 23, "", 1}}},
		{"123", []Item{
			{ItemNumber, 0, "123", 1},
			{ItemEOF, 3, "", 1}}},
		{"123.456", []Item{
			{ItemNumber, 0, "123.456", 1},
			{ItemEOF, 7, "", 1}}},
		{".456", []Item{
			{ItemDot, 0, ".", 1},
			{ItemNumber, 1, "456", 1},
			{ItemEOF, 4, "", 1}}},
		{"123.", []Item{
			{ItemError, 0, "number with trailing .", 1},
			{ItemEOF, 4, "", 1}}},
		{`""`, []Item{
			{ItemString, 0, `""`, 1},
			{ItemEOF, 2, "", 1}}},
		{`"string"`, []Item{
			{ItemString, 0, `"string"`, 1},
			{ItemEOF, 8, "", 1}}},
		{"//foo\n123", []Item{
			{ItemNumber, 6, "123", 2},
			{ItemEOF, 9, "", 2}}},
		{"andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", []Item{
			{ItemIdentifier, 0, "andy", 1},
			{ItemIdentifier, 5, "formless", 1},
			{ItemIdentifier, 14, "fo", 1},
			{ItemIdentifier, 17, "_", 1},
			{ItemIdentifier, 19, "_123", 1},
			{ItemIdentifier, 24, "_abc", 1},
			{ItemIdentifier, 29, "ab123", 1},
			{ItemIdentifier, 35, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", 2},
			{ItemEOF, 35 + 26*2 + 10 + 1, "", 2}}},
		{"and class else false for fun if nil or return super this true var while", []Item{
			{ItemAnd, 0, "and", 1},
			{ItemClass, 4, "class", 1},
			{ItemElse, 10, "else", 1},
			{ItemFalse, 15, "false", 1},
			{ItemFor, 21, "for", 1},
			{ItemFun, 25, "fun", 1},
			{ItemIf, 29, "if", 1},
			{ItemNil, 32, "nil", 1},
			{ItemOr, 36, "or", 1},
			{ItemReturn, 39, "return", 1},
			{ItemSuper, 46, "super", 1},
			{ItemThis, 52, "this", 1},
			{ItemTrue, 57, "true", 1},
			{ItemVar, 62, "var", 1},
			{ItemWhile, 66, "while", 1},
			{ItemEOF, 71, "", 1}}},
	}
	for _, tc := range cases {
		l := lex(tc.in)
		out := make([]Item, 0, len(tc.exp))
		for l.scan() {
			out = append(out, l.item())
		}
		if len(out) != len(tc.exp) {
			t.Errorf("len(lex(%s)) = %d, want %d", tc.in, len(out), len(tc.exp))
		}
		for i, outItem := range out {
			outStr := fmt.Sprintf("%#v", outItem)
			expStr := fmt.Sprintf("%#v", tc.exp[i])
			if outStr != expStr {
				t.Errorf("lex(%s)[%d] =\n%s; want\n%s", tc.in, i, outStr, expStr)
			}
		}
	}
}
