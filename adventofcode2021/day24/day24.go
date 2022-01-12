package day24

import (
	"bufio"
	"flag"
	"fmt"
	"strconv"
	"strings"
)

var pretty = flag.Bool("pretty", false, "")

func check(cond bool, format string, a ...interface{}) {
	if !cond {
		panic(fmt.Sprintf(format, a...))
	}
}

type range_ struct{ min, max int }

func (r *range_) Format(f fmt.State, verb rune) {
	fmt.Fprintf(f, "[%d..%d]", r.min, r.max)
}

func (r *range_) empty() bool {
	return r != nil && r.min > r.max
}

func (r *range_) copy_() *range_ {
	if r != nil {
		return &range_{r.min, r.max}
	}
	return nil
}

// (1,5) (1,3) = 1,3
// (1,2) (3,4) = nil
func intersect(a, b *range_) *range_ {
	r := a.copy_()
	switch {
	case a == nil:
		return b.copy_()
	case b == nil:
		return a.copy_()
	case b.min > a.min:
		r.min = b.min
	case b.max < a.max:
		r.max = b.max
	}
	return r
}

func (a *range_) sub(b *range_) *range_ {
	r := a.copy_()
	if b == nil || b.empty() || a == nil || a.empty() {
		return r
	}
	if r.max >= b.min && r.max <= b.max {
		r.max = b.min - 1
	}
	if r.min <= b.max && r.min >= b.min {
		r.min = b.max + 1
	}
	return r
}

func shift(a *range_, c int) *range_ {
	return &range_{a.min + c, a.max + c}
}

const numDigits = 14

// Conditions for a value on each digit
type conds [numDigits]*range_

func (c *conds) Format(f fmt.State, verb rune) {
	if c == nil {
		fmt.Fprintf(f, "none")
	}
	parts := []string{}
	for i, r := range *c {
		if r != nil {
			parts = append(parts, fmt.Sprintf("d%d:%v", i, r))
		}
	}
	if len(parts) == 0 {
		fmt.Fprintf(f, "all")
	} else {
		fmt.Fprintf(f, strings.Join(parts, ","))
	}
}

var allNums conds
var noNums *conds

func mergeConds(a, b *conds) *conds {
	r := allNums
	for i := 0; i < numDigits; i++ {
		r[i] = intersect((*a)[i], (*b)[i])
		if r[i].empty() {
			return noNums
		}
	}
	return &r
}

func (c *conds) max() int {
	r := 0
	for _, rng := range *c {
		r *= 10
		if rng == nil {
			r += 9
		} else {
			r += rng.max
		}
	}
	return r
}

func (c *conds) min() int {
	r := 0
	for _, rng := range *c {
		r *= 10
		if rng == nil {
			r += 1
		} else {
			r += rng.min
		}
	}
	return r
}

// a val = coeff*sub + digit + c subject to cond.
type val struct {
	coeff int
	sub   *val
	digit *int
	c     int
	cond  *conds
}

func (v *val) copy_() *val {
	r := *v
	r.cond = nil
	return &r
}

func (v *val) Format(f fmt.State, verb rune) {
	empty := true
	if v.c != 0 {
		fmt.Fprintf(f, "%d", v.c)
		empty = false
	}
	if v.digit != nil {
		if !empty {
			fmt.Fprintf(f, "+")
		}
		fmt.Fprintf(f, "d%d", *v.digit)
		empty = false
	}
	if v.sub != nil {
		if !empty {
			fmt.Fprintf(f, "+")
		}
		fmt.Fprintf(f, "%d(%z)", v.coeff, v.sub)
		empty = false
	}
	if empty {
		fmt.Fprintf(f, "0")
	}
	if verb != 'z' {
		fmt.Fprintf(f, "{%v}", v.cond)
	}
}

func numVal(num int) *val {
	return &val{c: num, cond: &allNums}
}

func copySub(a, b, r *val) {
	check(a.sub == nil || b.sub == nil, "invalid call to copySub")
	if a.sub != nil {
		r.sub = a.sub
		r.coeff = a.coeff
	}
	if b.sub != nil {
		r.sub = b.sub
		r.coeff = b.coeff
	}
}

func copyDigit(a, b, r *val) {
	check(a.digit == nil || b.digit == nil, "invalid call to copyDigit: %v, %v", a, b)
	if a.digit != nil {
		r.digit = a.digit
	}
	if b.digit != nil {
		r.digit = b.digit
	}
}

func add(a, b *val) []*val {
	r := &val{
		c: a.c + b.c,
	}
	copySub(a, b, r)
	copyDigit(a, b, r)
	return []*val{r}
}

func mul(a, b *val) []*val {
	check(b.digit == nil && b.sub == nil, "unexpected rhs to mul: %v", b)
	switch {
	case b.c == 1:
		return []*val{a.copy_()}
	case b.c == 0:
		return []*val{{c: 0}}
	case a.digit != nil || a.sub != nil:
		return []*val{{coeff: b.c, sub: a.copy_()}}
	default:
		return []*val{{c: a.c * b.c}}
	}
}

func mod(a, b *val) []*val {
	check(b.sub == nil && b.digit == nil, "unexpected rhs to mod: %v", b)
	if a.sub != nil {
		check(a.coeff == b.c, "unexpected lhs to mod: %v", a)
	}
	check(a.c < b.c-9, "unexpected lhs to mod: %v", a)
	return []*val{{digit: a.digit, c: a.c}}
}

func div(a, b *val) []*val {
	check(b.digit == nil && b.sub == nil, "unexpected rhs to div: %v", b)
	if b.c == 1 {
		return []*val{a.copy_()}
	}
	check(a.c < b.c-9, "unexpected lhs to div: %v", a)
	if a.sub == nil {
		return []*val{{c: 0}}
	}
	check(a.coeff == b.c, "unexpected lhs to div: %v", a)
	return []*val{a.sub.copy_()}
}

func apply(f func(a, b *val) []*val, as, bs []*val) []*val {
	rs := []*val{}
	for _, a := range as {
		for _, b := range bs {
			cond := mergeConds(a.cond, b.cond)
			if cond == nil {
				continue
			}
			ys := f(a, b)
			for _, y := range ys {
				if y.cond == nil {
					y.cond = cond
				} else {
					yc := mergeConds(cond, y.cond)
					if yc == nil {
						continue
					}
					y.cond = yc
				}
				rs = append(rs, y)
			}
		}
	}
	return rs
}

func eql(a, b *val) []*val {
	check(a.sub == nil, "unexpected lhs to eql: %v", a)
	check(b.sub == nil && b.c == 0, "unexpected rhs to eql: %v", b)
	if b.digit == nil {
		check(a.digit == nil, "unexpected lhs to eql: %v", a)
		if a.c == b.c {
			return []*val{numVal(1)}
		}
		return []*val{numVal(0)}
	}
	ar, br := shift(&range_{1, 9}, a.c), shift(&range_{1, 9}, b.c)
	eqr := intersect(ar, br)
	if eqr.empty() {
		return []*val{numVal(0)}
	}
	check(a.digit != nil && b.digit != nil, "unable to split eql: %v, %v", a, b)
	var eqc, nec conds
	eqc[*a.digit], eqc[*b.digit] = shift(eqr, -a.c), shift(eqr, -b.c)
	nec[*a.digit], nec[*b.digit] = shift(ar.sub(eqr), -a.c), shift(br.sub(eqr), -b.c)
	return []*val{{c: 0, cond: &nec}, {c: 1, cond: &eqc}}
}

type varName byte

var invVarName varName

func (n varName) id() int {
	if n == invVarName {
		return -1
	}
	return int(n - 'w')
}

func fromStr(in string) (varName, bool) {
	if len(in) == 1 && in[0] >= 'w' && in[0] <= 'z' {
		return varName(in[0]), true
	}
	return invVarName, false
}

type mem struct {
	rs [4][]*val
}

func newMem() *mem {
	r := mem{}
	for i, _ := range r.rs {
		r.rs[i] = []*val{numVal(0)}
	}
	return &r
}

func (m *mem) Format(f fmt.State, verb rune) {
	for i, v := range m.rs {
		fmt.Fprintf(f, "%c: %v\n", byte(i+'w'), v)
	}
}

func (m *mem) set(name string, v []*val) {
	n, ok := fromStr(name)
	check(ok, "expected variable name, got %s", name)
	m.rs[n.id()] = v
}

func (m *mem) get(in string) []*val {
	n, ok := fromStr(in)
	if ok {
		return m.rs[n.id()]
	}
	i, err := strconv.Atoi(in)
	check(err == nil, "invalid call to get: %s", in)
	return []*val{numVal(i)}
}

func findZeroConds(scanner *bufio.Scanner) *conds {
	m := newMem()
	place := 0
	line := 0
	for scanner.Scan() {
		line++
		fmt.Printf("line %d: %s\n", line, scanner.Text())
		ins := strings.Split(scanner.Text(), " ")
		var f func(a, b *val) []*val
		switch ins[0] {
		case "inp":
			d := place
			m.set(ins[1], []*val{{digit: &d, cond: &allNums}})
			place++
			continue
		case "mul":
			if ins[2] == "0" {
				m.set(ins[1], []*val{numVal(0)})
				continue
			}
			f = mul
		case "add":
			f = add
		case "div":
			f = div
		case "mod":
			f = mod
		case "eql":
			f = eql
		}
		a, b := m.get(ins[1]), m.get(ins[2])
		m.set(ins[1], apply(f, a, b))

		if *pretty {
			fmt.Printf("%10v", m)
		}
	}
	for _, v := range m.get("z") {
		if v.c == 0 && v.digit == nil && v.sub == nil {
			return v.cond
		}
	}
	return nil
}

func Run(scanner *bufio.Scanner, p1 bool) (int, error) {
	zeroc := findZeroConds(scanner)
	if p1 {
		return zeroc.max(), nil
	}
	return zeroc.min(), nil
}
