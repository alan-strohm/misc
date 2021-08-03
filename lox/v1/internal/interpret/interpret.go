package interpret

import (
	"fmt"
	"strconv"

	"github.com/alan-strohm/misc/lox/v1/internal/parse"
	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

type Type int

const (
	ERROR Type = iota
	STRING
	NUMBER
	BOOL
	NIL
)

type Value struct {
	v interface{}
	t Type
}

func (v *Value) String() string {
	return fmt.Sprintf("%v", v.v)
}

func newError(op token.Token, format string, args ...interface{}) *Value {
	return &Value{t: ERROR,
		v: fmt.Errorf("pos %d: %s", op.Pos, fmt.Sprintf(format, args...))}
}

func newValue(v interface{}) *Value {
	switch v.(type) {
	case bool:
		return &Value{v: v, t: BOOL}
	case float64:
		return &Value{v: v, t: NUMBER}
	case string:
		return &Value{v: v, t: STRING}
	case nil:
		return &Value{v: v, t: NIL}
	default:
		return &Value{v: fmt.Errorf("unexpected type %v", v), t: ERROR}
	}
}

func (v *Value) isTruthy() bool {
	switch c := v.v.(type) {
	case bool:
		return c
	case nil:
		return false
	}
	return true
}

func (v *Value) err() error {
	if err, ok := v.v.(error); ok {
		return err
	}
	return nil
}

func numberOp(op token.Token, x, y float64) *Value {
	switch op.Type {
	case token.GTR:
		return newValue(x > y)
	case token.GEQ:
		return newValue(x >= y)
	case token.LSS:
		return newValue(x < y)
	case token.LEQ:
		return newValue(x <= y)
	case token.ADD:
		return newValue(x + y)
	case token.SUB:
		return newValue(x - y)
	case token.QUO:
		return newValue(x / y)
	case token.MUL:
		return newValue(x * y)
	}
	panic(fmt.Sprintf("operator [%s] is not a binary number operator", op.Type))
}

func (x *Value) unary(op token.Token) *Value {
	if x.err() != nil {
		return x
	}
	switch op.Type {
	case token.NOT:
		return newValue(!x.isTruthy())
	case token.SUB:
		f, ok := x.v.(float64)
		if ok {
			return newValue(-f)
		}
		return newError(op, "operand to %s must be a number", op.Type)
	}
	panic(fmt.Sprintf("non binary operator: %s", op.Type))
}

func (x *Value) binary(op token.Token, y *Value) *Value {
	if x.err() != nil {
		return x
	}
	if y.err() != nil {
		return y
	}

	switch op.Type {
	case token.GTR, token.GEQ, token.LSS, token.LEQ, token.SUB, token.QUO, token.MUL, token.ADD:
		fx, xok := x.v.(float64)
		fy, yok := y.v.(float64)
		if xok && yok {
			return numberOp(op, fx, fy)
		}
		if op.Type != token.ADD {
			return newError(op, "operands to %s must be numbers", op.Type)
		}
		sx, xok := x.v.(string)
		sy, yok := y.v.(string)
		if xok && yok {
			return newValue(sx + sy)
		}
		return newError(op, "operands to %s must be numbers or strings", op.Type)
	case token.EQL:
		return newValue(isEqual(x, y))
	case token.NEQ:
		return newValue(!isEqual(x, y))
	}
	panic(fmt.Sprintf("operator [%s] is not a binary operator", op.Type))
}

func isEqual(x, y *Value) bool {
	return x.v == y.v
}

type visitor struct {
	stack []*Value
}

func (i *visitor) push(v *Value) {
	i.stack = append(i.stack, v)
}
func (i *visitor) peek() *Value {
	return i.stack[len(i.stack)-1]
}
func (i *visitor) pop() *Value {
	r := i.peek()
	i.stack = i.stack[0 : len(i.stack)-1]
	return r
}

func (i *visitor) evaluate(e parse.Expr) *Value {
	parse.ExprAcceptFullVisitor(e, i)
	r := i.pop()
	return r
}

func (i *visitor) VisitParenExpr(e *parse.ParenExpr) {
	parse.ExprAcceptFullVisitor(e.X, i)
}

func (i *visitor) VisitBinaryExpr(e *parse.BinaryExpr) {
	x := i.evaluate(e.X)
	y := i.evaluate(e.Y)
	i.push(x.binary(e.Op, y))
}

func (i *visitor) VisitUnaryExpr(e *parse.UnaryExpr) {
	v := i.evaluate(e.X)
	i.push(v.unary(e.Op))
}

func (i *visitor) VisitBasicLit(e *parse.BasicLit) {
	switch e.Value.Type {
	case token.STRING:
		s, err := strconv.Unquote(e.Value.Val)
		if err == nil {
			i.push(newValue(s))
		} else {
			i.push(newError(e.Value, "invalid string: %s", err))
		}
	case token.NUMBER:
		f, err := strconv.ParseFloat(e.Value.Val, 64)
		if err == nil {
			i.push(newValue(f))
		} else {
			i.push(newError(e.Value, "invalid number: %s", err))
		}
	case token.FALSE:
		i.push(newValue(false))
	case token.TRUE:
		i.push(newValue(true))
	case token.NIL:
		i.push(newValue(nil))
	}
}

func Interpret(e parse.Expr) (*Value, error) {
	v := &visitor{}
	r := v.evaluate(e)
	return r, r.err()
}
