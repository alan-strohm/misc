package interpret

import (
	"fmt"
	"strings"
	"testing"

	"github.com/alan-strohm/misc/lox/v1/internal/parse"
)

func TestInterpret(t *testing.T) {
	cases := []struct {
		in, exp, err string
	}{
		{"1", "1", ""},
		{"(5 - (3 - 1)) + -1", "2", ""},
		{"5 < 4", "false", ""},
		{"5 > 8 / 2", "true", ""},
		{"5 == 3 + 2", "true", ""},
		{"false == 3 >= 3", "false", ""},
		{"false != 3 > 2", "true", ""},
		{"1 / 3", "0.3333333333333333", ""},
		{`"a" + "b"`, "ab", ""},
		{`"5" == 5`, "false", ""},
		{"2 + 3 * 4", "14", ""},
		{"20 - 3 * 4", "8", ""},
		{"2 + 6 / 3", "4", ""},
		{"2 - 6 / 3", "0", ""},
		{"false == 2 < 1", "true", ""},
		{"false == 1 > 2", "true", ""},
		{"false == 2 <= 1", "true", ""},
		{"false == 1 >= 2", "true", ""},
		{"1 - 1", "0", ""},
		{"1 -1", "0", ""},
		{"1- 1", "0", ""},
		{"1-1", "0", ""},
		{"(2 * (6 - (2 + 2)))", "4", ""},
		{"1.2 - 1.2", "0", ""},
		{"nil != nil", "false", ""},
		{"true != true", "false", ""},
		{"true != false", "true", ""},
		{"1 != 1", "false", ""},
		{"1 != 2", "true", ""},
		{`"str" != "str"`, "false", ""},
		{`"str" != "ing"`, "true", ""},
		{"nil != false", "true", ""},
		{"false != 0", "true", ""},
		{`0 != "0"`, "true", ""},
		{"!true", "false", ""},
		{"!false", "true", ""},
		{"!!true", "true", ""},
		{"!123", "false", ""},
		{"!0", "false", ""},
		{"!nil", "true", ""},
		{`!""`, "false", ""},
		{`-"s"`, "", "pos 0: operand to - must be a number"},
	}
	for _, tc := range cases {
		e, err := parse.ParseExpr(tc.in)
		if err != nil {
			t.Errorf("ParseExpr(%s) = %s, want nil", tc.in, err)
		}
		v, err := Interpret(e)
		if err == nil {
			if fmt.Sprintf("%v", v) != tc.exp {
				t.Errorf("Interpret(%s) = %v, want %s", tc.in, v, tc.exp)
			}
		} else {
			if tc.err == "" {
				t.Errorf("Interpret(%s) = %s, want nil", tc.in, err)
			} else if !strings.Contains(err.Error(), tc.err) {
				t.Errorf("Interpret(%s) = %s, want %s", tc.in, err, tc.err)
			}
		}
	}
}
