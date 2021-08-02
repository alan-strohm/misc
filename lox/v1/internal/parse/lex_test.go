package parse

import (
	"fmt"
	"testing"
)

func line1I(typ itemType, val string) item {
	return item{typ: typ, val: val, line: 1}
}
func eofI(line int) item {
	return item{typ: itemEOF, line: line}
}

func TestLex(t *testing.T) {
	cases := []struct {
		in  string
		exp []item
	}{
		{"", []item{eofI(1)}},
		{"\n", []item{eofI(2)}},
		{" \n\r\t", []item{eofI(2)}},
		{"(){};,+-*!===<=>=!=<>/.", []item{
			line1I(itemLeftParen, "("),
			line1I(itemRightParen, ")"),
			line1I(itemLeftBrace, "{"),
			line1I(itemRightBrace, "}"),
			line1I(itemSemicolon, ";"),
			line1I(itemComma, ","),
			line1I(itemPlus, "+"),
			line1I(itemMinus, "-"),
			line1I(itemStar, "*"),
			line1I(itemBangEqual, "!="),
			line1I(itemEqualEqual, "=="),
			line1I(itemLessEqual, "<="),
			line1I(itemGreaterEqual, ">="),
			line1I(itemBangEqual, "!="),
			line1I(itemLess, "<"),
			line1I(itemGreater, ">"),
			line1I(itemSlash, "/"),
			line1I(itemDot, "."),
			eofI(1)}},
		{"123", []item{line1I(itemNumber, "123"), eofI(1)}},
		{"123.456", []item{line1I(itemNumber, "123.456"), eofI(1)}},
		{".456", []item{line1I(itemDot, "."), line1I(itemNumber, "456"), eofI(1)}},
		{"123.", []item{line1I(itemError, "number with trailing ."), eofI(1)}},
		{`""`, []item{line1I(itemString, `""`), eofI(1)}},
		{`"string"`, []item{line1I(itemString, `"string"`), eofI(1)}},
		{"//foo\n123", []item{{typ: itemNumber, val: "123", line: 2}, eofI(2)}},
		{"andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", []item{
			line1I(itemIdentifier, "andy"),
			line1I(itemIdentifier, "formless"),
			line1I(itemIdentifier, "fo"),
			line1I(itemIdentifier, "_"),
			line1I(itemIdentifier, "_123"),
			line1I(itemIdentifier, "_abc"),
			line1I(itemIdentifier, "ab123"),
			{typ: itemIdentifier, val: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", line: 2},
			eofI(2)}},
		{"and class else false for fun if nil or return super this true var while", []item{
			line1I(itemAnd, "and"),
			line1I(itemClass, "class"),
			line1I(itemElse, "else"),
			line1I(itemFalse, "false"),
			line1I(itemFor, "for"),
			line1I(itemFun, "fun"),
			line1I(itemIf, "if"),
			line1I(itemNil, "nil"),
			line1I(itemOr, "or"),
			line1I(itemReturn, "return"),
			line1I(itemSuper, "super"),
			line1I(itemThis, "this"),
			line1I(itemTrue, "true"),
			line1I(itemVar, "var"),
			line1I(itemWhile, "while"),
			eofI(1)}},
	}
	for _, tc := range cases {
		l := lex(tc.in)
		out := make([]item, 0, len(tc.exp))
		for l.scan() {
			out = append(out, l.item())
		}
		outStr := fmt.Sprintf("%#v", out)
		expStr := fmt.Sprintf("%#v", tc.exp)
		if outStr != expStr {
			t.Errorf("lex(%s) =\n%s; want\n%s", tc.in, outStr, expStr)
		}
	}
}
