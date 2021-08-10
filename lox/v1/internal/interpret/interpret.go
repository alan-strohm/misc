package interpret

import (
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/alan-strohm/misc/lox/v1/internal/ast"
	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

type Type int

const (
	ERROR Type = iota
	STRING
	NUMBER
	BOOL
	NIL
	CALLABLE
)

type Value struct {
	v interface{}
	t Type
}

func (v *Value) String() string {
	switch v.v.(type) {
	case nil:
		return "nil"
	default:
		return fmt.Sprintf("%v", v.v)
	}
}

func newError(pos token.Pos, format string, args ...interface{}) *Value {
	return &Value{t: ERROR,
		v: fmt.Errorf("pos %d: %s", pos, fmt.Sprintf(format, args...))}
}

type callable struct {
	arity int
	impl  func([]*Value) *Value
	env   *env
	name  string
}

func (c callable) String() string {
	if c.name == "" {
		return "<native fn>"
	}
	return fmt.Sprintf("<fn %s>", c.name)
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
	case callable:
		return &Value{v: v, t: CALLABLE}
	default:
		return &Value{v: fmt.Errorf("unexpected type %#v", v), t: ERROR}
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
		return newError(op.Pos, "operand to %s must be a number", op.Type)
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
			return newError(op.Pos, "operands to %s must be numbers", op.Type)
		}
		sx, xok := x.v.(string)
		sy, yok := y.v.(string)
		if xok && yok {
			return newValue(sx + sy)
		}
		return newError(op.Pos, "operands to %s must be numbers or strings", op.Type)
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
	parent *env
	ids    map[string]*Value
}

func newEnv(parent *env) *env {
	return &env{parent: parent, ids: make(map[string]*Value)}
}

func (e *env) define(id string, v *Value) {
	c := *v
	e.ids[id] = &c
}

func (e *env) lookup(id string) *Value {
	v, ok := e.ids[id]
	if ok {
		return v
	}
	if e.parent == nil {
		return nil
	}
	return e.parent.lookup(id)
}

func (e *env) String() string {
	return fmt.Sprintf("%#v\n  %s", e.ids, e.parent)
}

var globals = newEnv(nil)

func init() {
	globals.define("clock", newValue(callable{arity: 0, impl: func(_ []*Value) *Value {
		return newValue(float64(time.Now().UnixNano()) / 1e9)
	}}))
}

type Interpreter struct {
	stack  []*Value
	errors []error
	env    *env
	ret    *Value // nil unless we are returning..
}

// Should we break any current loop?
func (i *Interpreter) shouldBreak() bool {
	return i.ret != nil || i.err() != nil
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

func (i *Interpreter) evaluate(e ast.Expr) *Value {
	ast.ExprAcceptFullVisitor(e, i)
	r := i.pop()
	if r.err() != nil {
		i.errors = append(i.errors, r.err())
	}
	return r
}

func (i *Interpreter) VisitParenExpr(e *ast.ParenExpr) {
	ast.ExprAcceptFullVisitor(e.X, i)
}

func (i *Interpreter) VisitBinaryExpr(e *ast.BinaryExpr) {
	x := i.evaluate(e.X)
	// Short-circuit logical operators.
	if (e.Op.Type == token.OR && x.isTruthy()) ||
		(e.Op.Type == token.AND && !x.isTruthy()) {
		i.push(x)
		return
	}
	y := i.evaluate(e.Y)
	if e.Op.Type == token.OR || e.Op.Type == token.AND {
		i.push(y)
		return
	}
	i.push(x.binary(e.Op, y))
}

func (i *Interpreter) VisitUnaryExpr(e *ast.UnaryExpr) {
	v := i.evaluate(e.X)
	i.push(v.unary(e.Op))
}

func (i *Interpreter) setEnv(e *env) (out *env) {
	out, i.env = i.env, e
	return
}

func (i *Interpreter) restoreEnv(e *env) {
	i.env = e
}

func (i *Interpreter) VisitCallExpr(e *ast.CallExpr) {
	fun, ok := i.evaluate(e.Fun).v.(callable)
	if !ok {
		i.push(newError(e.Lparen, "Can only call functions and classes."))
		return
	}
	if fun.arity != len(e.Args) {
		i.push(newError(e.Rparen, fmt.Sprintf("expected %d args to function call, got %d", fun.arity, len(e.Args))))
		return
	}

	args := make([]*Value, len(e.Args))
	for ix, arg := range e.Args {
		args[ix] = i.evaluate(arg)
		if args[ix].err() != nil {
			i.push(args[ix])
			return
		}
	}
	i.push(fun.impl(args))
}

func (i *Interpreter) VisitBasicLit(e *ast.BasicLit) {
	switch e.Value.Type {
	case token.STRING:
		i.push(newValue(strings.Replace(e.Value.Val, "\"", "", -1)))
	case token.NUMBER:
		f, err := strconv.ParseFloat(e.Value.Val, 64)
		if err == nil {
			i.push(newValue(f))
		} else {
			i.push(newError(e.Value.Pos, "invalid number: %s", err))
		}
	case token.FALSE:
		i.push(newValue(false))
	case token.TRUE:
		i.push(newValue(true))
	case token.NIL:
		i.push(newValue(nil))
	}
}

func (i *Interpreter) VisitIdent(x *ast.Ident) {
	v := i.env.lookup(x.Tok.Val)
	if v == nil {
		i.push(newError(x.Tok.Pos, "reference to undefined identifier '%s'", x.Tok.Val))
		return
	}
	i.push(v)
}

func (i *Interpreter) VisitAssign(x *ast.Assign) {
	lhs := i.env.lookup(x.Name.Val)
	if lhs == nil {
		i.push(newError(x.Name.Pos, "assignment to undefined identifier '%s'", x.Name.Val))
		return
	}
	rhs := i.evaluate(x.Value)
	*lhs = *rhs
	i.push(lhs)
}

func (i *Interpreter) VisitExprStmt(x *ast.ExprStmt) {
	i.evaluate(x.X)
}

func (i *Interpreter) VisitPrintStmt(x *ast.PrintStmt) {
	v := i.evaluate(x.X)
	if len(i.errors) == 0 {
		fmt.Println(v)
	}
}

func (i *Interpreter) VisitVarDecl(x *ast.VarDecl) {
	v := newValue(nil)
	if x.Value != nil {
		v = i.evaluate(x.Value)
	}
	i.env.define(x.Name.Val, v)
}

func (i *Interpreter) VisitFunDecl(x *ast.FunDecl) {
	closure := i.env
	i.env.define(x.Name.Val, newValue(callable{
		arity: len(x.Params),
		name:  x.Name.Val,
		impl: func(args []*Value) *Value {
			return i.executeFunction(x, args, closure)
		}}))
}

func (i *Interpreter) executeBlock(block []ast.Stmt) error {
	for _, s := range block {
		i.execute(s)
		if i.shouldBreak() {
			return i.err()
		}
	}
	return i.err()
}

func (i *Interpreter) VisitBlockStmt(x *ast.BlockStmt) {
	oldEnv := i.setEnv(newEnv(i.env))
	defer i.restoreEnv(oldEnv)

	_ = i.executeBlock(x.List)
}

func (i *Interpreter) VisitIfStmt(x *ast.IfStmt) {
	if i.evaluate(x.Cond).isTruthy() {
		i.execute(x.Body)
	} else if x.Else != nil {
		i.execute(x.Else)
	}
}

func (i *Interpreter) VisitWhileStmt(x *ast.WhileStmt) {
	for i.evaluate(x.Cond).isTruthy() && !i.shouldBreak() {
		i.execute(x.Body)
	}
}

func (i *Interpreter) VisitForStmt(x *ast.ForStmt) {
	oldEnv := i.setEnv(newEnv(i.env))
	defer i.restoreEnv(oldEnv)

	init := func() {
		if x.Init != nil {
			i.execute(x.Init)
		}
	}
	cond := func() bool {
		if x.Cond == nil {
			return true
		}
		return i.evaluate(x.Cond).isTruthy()
	}
	post := func() {
		if x.Post != nil {
			i.evaluate(x.Post)
		}
	}
	for init(); cond() && !i.shouldBreak(); post() {
		i.execute(x.Body)
	}
}

func (i *Interpreter) VisitReturnStmt(x *ast.ReturnStmt) {
	v := newValue(nil)
	if x.Result != nil {
		v = i.evaluate(x.Result)
	}
	i.ret = v
}

func (i *Interpreter) executeFunction(decl *ast.FunDecl, args []*Value, closure *env) *Value {
	oldEnv := i.setEnv(newEnv(closure))
	defer i.restoreEnv(oldEnv)

	for idx, param := range decl.Params {
		i.env.define(param.Val, args[idx])
	}
	i.execute(decl.Body)
	if i.ret != nil {
		r := i.ret
		i.ret = nil
		return r
	}
	return newValue(nil)
}

func (i *Interpreter) execute(x ast.Stmt) error {
	ast.StmtAcceptFullVisitor(x, i)
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
	i := &Interpreter{env: globals}
	return i
}

func InterpretExpr(e ast.Expr) (*Value, error) {
	r := New().evaluate(e)
	return r, r.err()
}

func (i *Interpreter) Interpret(program []ast.Stmt) error {
	return i.executeBlock(program)
}
