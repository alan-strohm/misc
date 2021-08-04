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

type env struct {
	stack []map[string]*Value
}

func (e *env) push() {
	e.stack = append(e.stack, make(map[string]*Value))
}
func (e *env) pop() {
	e.stack = e.stack[0 : len(e.stack)-1]
}
func (e *env) peek() map[string]*Value {
	return e.stack[len(e.stack)-1]
}

func (e *env) define(id string, v *Value) {
	e.peek()[id] = v
}

func (e *env) lookup(id string) *Value {
	for i := len(e.stack) - 1; i >= 0; i-- {
		if v, ok := e.stack[i][id]; ok {
			return v
		}
	}
	return nil
}

type Interpreter struct {
	stack  []*Value
	errors []error
	env    env
}

func (i *Interpreter) push(v *Value) {
	i.stack = append(i.stack, v)
}
func (i *Interpreter) peek() *Value {
	return i.stack[len(i.stack)-1]
}
func (i *Interpreter) pop() *Value {
	r := i.peek()
	i.stack = i.stack[0 : len(i.stack)-1]
	return r
}

func (i *Interpreter) evaluate(e parse.Expr) *Value {
	parse.ExprAcceptFullVisitor(e, i)
	r := i.pop()
	if r.err() != nil {
		i.errors = append(i.errors, r.err())
	}
	return r
}

func (i *Interpreter) VisitParenExpr(e *parse.ParenExpr) {
	parse.ExprAcceptFullVisitor(e.X, i)
}

func (i *Interpreter) VisitBinaryExpr(e *parse.BinaryExpr) {
	x := i.evaluate(e.X)
	y := i.evaluate(e.Y)
	i.push(x.binary(e.Op, y))
}

func (i *Interpreter) VisitUnaryExpr(e *parse.UnaryExpr) {
	v := i.evaluate(e.X)
	i.push(v.unary(e.Op))
}

func (i *Interpreter) VisitBasicLit(e *parse.BasicLit) {
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

func (i *Interpreter) VisitIdent(x *parse.Ident) {
	v := i.env.lookup(x.Tok.Val)
	if v == nil {
		i.push(newError(x.Tok, "reference to undefined identifier '%s'", x.Tok.Val))
		return
	}
	i.push(v)
}

func (i *Interpreter) VisitAssign(x *parse.Assign) {
	lhs := i.env.lookup(x.Name.Val)
	if lhs == nil {
		i.push(newError(x.Name, "assignment to undefined identifier '%s'", x.Name.Val))
		return
	}
	rhs := i.evaluate(x.Value)
	*lhs = *rhs
	i.push(lhs)
}

func (i *Interpreter) VisitExprStmt(x *parse.ExprStmt) {
	i.evaluate(x.X)
}

func (i *Interpreter) VisitPrintStmt(x *parse.PrintStmt) {
	v := i.evaluate(x.X)
	if len(i.errors) == 0 {
		fmt.Println(v)
	}
}

func (i *Interpreter) VisitVarStmt(x *parse.VarStmt) {
	v := newValue(nil)
	if x.Value != nil {
		v = i.evaluate(x.Value)
	}
	i.env.define(x.Name.Val, v)
}

func (i *Interpreter) executeBlock(block []parse.Stmt) error {
	for _, s := range block {
		if err := i.execute(s); err != nil {
			return err
		}
	}
	return nil
}

func (i *Interpreter) VisitBlockStmt(x *parse.BlockStmt) {
	i.env.push()
	defer i.env.pop()

	_ = i.executeBlock(x.List)
}

func (i *Interpreter) execute(x parse.Stmt) error {
	parse.StmtAcceptFullVisitor(x, i)
	return i.err()
}

func (i *Interpreter) err() error {
	switch len(i.errors) {
	case 0:
		return nil
	case 1:
		return i.errors[0]
	}
	return fmt.Errorf("%s (and %d more errors)", i.errors[0], len(i.errors)-1)
}

func New() *Interpreter {
	i := &Interpreter{}
	i.env.push()
	return i
}

func InterpretExpr(e parse.Expr) (*Value, error) {
	r := New().evaluate(e)
	return r, r.err()
}

func (i *Interpreter) Interpret(program []parse.Stmt) error {
	return i.executeBlock(program)
}
