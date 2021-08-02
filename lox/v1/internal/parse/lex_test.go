package parse

import (
	"fmt"
	"testing"
)

func TestLex(t *testing.T) {
	cases := []struct {
		in  string
		exp []item
	}{
		{"", []item{{itemEOF, 0, "", 1}}},
		{"\n", []item{{itemEOF, 1, "", 2}}},
		{" \n\r\t", []item{{itemEOF, 4, "", 2}}},
		{"(){};,+-*!===<=>=!=<>/.", []item{
			{itemLeftParen, 0, "(", 1},
			{itemRightParen, 1, ")", 1},
			{itemLeftBrace, 2, "{", 1},
			{itemRightBrace, 3, "}", 1},
			{itemSemicolon, 4, ";", 1},
			{itemComma, 5, ",", 1},
			{itemPlus, 6, "+", 1},
			{itemMinus, 7, "-", 1},
			{itemStar, 8, "*", 1},
			{itemBangEqual, 9, "!=", 1},
			{itemEqualEqual, 11, "==", 1},
			{itemLessEqual, 13, "<=", 1},
			{itemGreaterEqual, 15, ">=", 1},
			{itemBangEqual, 17, "!=", 1},
			{itemLess, 19, "<", 1},
			{itemGreater, 20, ">", 1},
			{itemSlash, 21, "/", 1},
			{itemDot, 22, ".", 1},
			{itemEOF, 23, "", 1}}},
		{"123", []item{
			{itemNumber, 0, "123", 1},
			{itemEOF, 3, "", 1}}},
		{"123.456", []item{
			{itemNumber, 0, "123.456", 1},
			{itemEOF, 7, "", 1}}},
		{".456", []item{
			{itemDot, 0, ".", 1},
			{itemNumber, 1, "456", 1},
			{itemEOF, 4, "", 1}}},
		{"123.", []item{
			{itemError, 0, "number with trailing .", 1},
			{itemEOF, 4, "", 1}}},
		{`""`, []item{
			{itemString, 0, `""`, 1},
			{itemEOF, 2, "", 1}}},
		{`"string"`, []item{
			{itemString, 0, `"string"`, 1},
			{itemEOF, 8, "", 1}}},
		{"//foo\n123", []item{
			{itemNumber, 6, "123", 2},
			{itemEOF, 9, "", 2}}},
		{"andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", []item{
			{itemIdentifier, 0, "andy", 1},
			{itemIdentifier, 5, "formless", 1},
			{itemIdentifier, 14, "fo", 1},
			{itemIdentifier, 17, "_", 1},
			{itemIdentifier, 19, "_123", 1},
			{itemIdentifier, 24, "_abc", 1},
			{itemIdentifier, 29, "ab123", 1},
			{itemIdentifier, 35, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", 2},
			{itemEOF, 35 + 26*2 + 10 + 1, "", 2}}},
		{"and class else false for fun if nil or return super this true var while", []item{
			{itemAnd, 0, "and", 1},
			{itemClass, 4, "class", 1},
			{itemElse, 10, "else", 1},
			{itemFalse, 15, "false", 1},
			{itemFor, 21, "for", 1},
			{itemFun, 25, "fun", 1},
			{itemIf, 29, "if", 1},
			{itemNil, 32, "nil", 1},
			{itemOr, 36, "or", 1},
			{itemReturn, 39, "return", 1},
			{itemSuper, 46, "super", 1},
			{itemThis, 52, "this", 1},
			{itemTrue, 57, "true", 1},
			{itemVar, 62, "var", 1},
			{itemWhile, 66, "while", 1},
			{itemEOF, 71, "", 1}}},
	}
	for _, tc := range cases {
		l := lex(tc.in)
		out := make([]item, 0, len(tc.exp))
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
