package day18

import "testing"

func TestEvalStringExpr(t *testing.T) {
	testCases := []struct {
		expr    string
		eqWant  int
		revWant int
		err     error
	}{
		{"5", 5, 5, nil}, {"1+2", 3, 3, nil}, {"2*3", 6, 6, nil},
		{"1 + 2", 3, 3, nil}, {"1 + 2 * 3", 9, 9, nil},
		{"2 * 3 + 2", 8, 10, nil},
		{"1 + 2 * 3 + 4 * 5 + 6", 71, 231, nil},
		{"1 + (2 * 3) + (4 * (5 + 6))", 51, 51, nil},
		{"2 * 3 + (4 * 5)", 26, 46, nil},
		{"5 + (8 * 3 + 9 + 3 * 4 * 3)", 437, 1445, nil},
		{"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240, 669060, nil},
		{"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632, 23340, nil},
	}
	for _, tc := range testCases {
		nEq, err := EvalStringExpr(tc.expr, eqPrecedence)
		if (err == nil) != (tc.err == nil) || (err == nil && nEq != tc.eqWant) {
			t.Errorf("EvalStringExpr(%s, eq) = %d, %v, want %d, %v", tc.expr, nEq, err, tc.eqWant, tc.err)
		}
		nRev, err := EvalStringExpr(tc.expr, revPrecedence)
		if (err == nil) != (tc.err == nil) || (err == nil && nRev != tc.revWant) {
			t.Errorf("EvalStringExpr(%s, rev) = %d, %v, want %d, %v", tc.expr, nRev, err, tc.revWant, tc.err)
		}
	}
}

func TestSumFileEq(t *testing.T) {
	n, err := SumFile("./input.txt", eqPrecedence)
	if err != nil {
		t.Errorf("unexpected error: %s", err)
	}
	if n != 21347713555555 {
		t.Errorf("got %d, want 21347713555555", n)
	}
}
func TestSumFileRev(t *testing.T) {
	n, err := SumFile("./input.txt", revPrecedence)
	if err != nil {
		t.Errorf("unexpected error: %s", err)
	}
	if n != 275011754427339 {
		t.Errorf("got %d, want 275011754427339", n)
	}
}
