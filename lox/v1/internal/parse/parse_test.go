package parse

import (
	"fmt"
	"testing"

	"github.com/alan-strohm/misc/lox/v1/internal/ast"
)

func TestParseProgram(t *testing.T) {
	cases := []struct {
		in  string
		exp []ast.Stmt
	}{
		{"  var x = 1;", []ast.Stmt{
			&ast.VarDecl{},
		}},
	}
	for _, tc := range cases {
		stmts, err := ParseProgram(tc.in)
		if err != nil {
			t.Errorf(`ParseProgram("%s") = %s, want nil`, tc.in, err)
		}
		if len(stmts) != len(tc.exp) {
			t.Errorf(`len(ParseProgram("%s")) = %d, want %d`, tc.in, len(stmts), len(tc.exp))
		}
		for i, s := range stmts {
			if fmt.Sprintf("%#v", tc.exp) != fmt.Sprintf("%#v", s) {
				t.Errorf(`ParseProgram("%s")[%d] = %#v, want %#v`, tc.in, i, s, tc.exp)
			}
		}
	}
}
