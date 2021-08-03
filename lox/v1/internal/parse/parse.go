package parse

import (
	"fmt"

	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

type Node interface {
	Pos() token.Pos // position of the first character belonging to the node
	End() token.Pos // position of the first character immediately after the node.
}

type Expr interface {
	Node
	exprNode()
}

type Stmt interface {
	Node
	stmtNode()
}

type (
	BadExpr struct {
		From, To token.Pos
	}

	Ident struct {
		Tok token.Token
	}

	// A BinaryExpr node represents a binary expression.
	BinaryExpr struct {
		X  Expr        // left operand
		Op token.Token // operator
		Y  Expr        // right operand
	}

	// A UnaryExpr node represents a unary expression.
	UnaryExpr struct {
		Op token.Token // operator
		X  Expr        // operand
	}

	// A BasicLit node represents a literal of basic type.
	//
	// Note that for token.STRING tokens, the literal is stored with its quotes.
	BasicLit struct {
		Value token.Token // The value
	}

	// A ParenExpr node represents a parenthesized expression.
	ParenExpr struct {
		Lparen token.Pos // position of "("
		X      Expr
		Rparen token.Pos // position of ")"
	}

	PartialExprVisitor interface {
		VisitExpr(x Expr)
	}
	IdentVisitor interface {
		VisitIdent(x *Ident)
	}
	BinaryExprVisitor interface {
		VisitBinaryExpr(x *BinaryExpr)
	}
	UnaryExprVisitor interface {
		VisitUnaryExpr(x *UnaryExpr)
	}
	BasicLitVisitor interface {
		VisitBasicLit(x *BasicLit)
	}
	ParenExprVisitor interface {
		VisitParenExpr(x *ParenExpr)
	}
	FullExprVisitor interface {
		IdentVisitor
		BinaryExprVisitor
		UnaryExprVisitor
		BasicLitVisitor
		ParenExprVisitor
	}
)

// Call the appropriate visitor method on v.
//
// When we add new types of expression, existing uses of ExprAcceptFullVisitor
// will no longer compile until their visitors are updated.  If, instead, you
// want a default visit function to be called, use ExprAcceptPartialVisitor.
func ExprAcceptFullVisitor(e Expr, v FullExprVisitor) {
	exprAcceptVisitor(e, v)
}

// If v implements a visitor for e's type (e.g. BinaryExprVisitor), call the
// appropriate visitor method (e.g. VisitBinaryExpr).  Otherwise, call VisitExpr.
func ExprAcceptPartialVisitor(e Expr, v PartialExprVisitor) {
	exprAcceptVisitor(e, v)
}

func exprAcceptVisitor(e Expr, v interface{}) {
	switch t := e.(type) {
	case *Ident:
		n, ok := v.(IdentVisitor)
		if ok {
			n.VisitIdent(t)
			return
		}
	case *BinaryExpr:
		n, ok := v.(BinaryExprVisitor)
		if ok {
			n.VisitBinaryExpr(t)
			return
		}
	case *UnaryExpr:
		n, ok := v.(UnaryExprVisitor)
		if ok {
			n.VisitUnaryExpr(t)
			return
		}
	case *BasicLit:
		n, ok := v.(BasicLitVisitor)
		if ok {
			n.VisitBasicLit(t)
			return
		}
	case *ParenExpr:
		n, ok := v.(ParenExprVisitor)
		if ok {
			n.VisitParenExpr(t)
			return
		}
	}
	v.(PartialExprVisitor).VisitExpr(e)
}

func (x *BadExpr) Pos() token.Pos    { return x.From }
func (x *Ident) Pos() token.Pos      { return x.Tok.Pos }
func (x *BasicLit) Pos() token.Pos   { return x.Value.Pos }
func (x *BinaryExpr) Pos() token.Pos { return x.X.Pos() }
func (x *ParenExpr) Pos() token.Pos  { return x.Lparen }
func (x *UnaryExpr) Pos() token.Pos  { return x.Op.Pos }

func (x *BadExpr) End() token.Pos    { return x.To }
func (x *Ident) End() token.Pos      { return x.Tok.Pos + token.Pos(len(x.Tok.Val)) }
func (x *BasicLit) End() token.Pos   { return x.Value.Pos + token.Pos(len(x.Value.Val)) }
func (x *BinaryExpr) End() token.Pos { return x.Y.End() }
func (x *ParenExpr) End() token.Pos  { return x.Rparen }
func (x *UnaryExpr) End() token.Pos  { return x.X.End() }

// exprNode() ensures that only expression/type nodes can be
// assigned to an Expr.
//
func (*BadExpr) exprNode()    {}
func (*Ident) exprNode()      {}
func (*BasicLit) exprNode()   {}
func (*ParenExpr) exprNode()  {}
func (*UnaryExpr) exprNode()  {}
func (*BinaryExpr) exprNode() {}

// ----------------------------------------------------------------------------
// Statements

// A statement is represented by a tree consisting of one
// or more of the following concrete statement nodes.
//
type (
	// A BadStmt node is a placeholder for statements containing
	// syntax errors for which no correct statement nodes can be
	// created.
	//
	BadStmt struct {
		From, To token.Pos // position range of bad statement
	}

	// An ExprStmt node represents a (stand-alone) expression
	// in a statement list.
	//
	ExprStmt struct {
		X Expr // expression
	}

	// A PrintStmt node represents a print statement.
	//
	PrintStmt struct {
		X Expr // expression
	}

	// A VarStmt node represents a var statement.
	//
	VarStmt struct {
		Name  token.Token
		Value Expr // initial value; or nil
	}

	PartialStmtVisitor interface {
		VisitStmt(x Stmt)
	}
	ExprStmtVisitor interface {
		VisitExprStmt(x *ExprStmt)
	}
	PrintStmtVisitor interface {
		VisitPrintStmt(x *PrintStmt)
	}
	VarStmtVisitor interface {
		VisitVarStmt(x *VarStmt)
	}
	FullStmtVisitor interface {
		ExprStmtVisitor
		PrintStmtVisitor
		VarStmtVisitor
	}
)

// Pos and End implementations for statement nodes.

func (s *BadStmt) Pos() token.Pos   { return s.From }
func (s *ExprStmt) Pos() token.Pos  { return s.X.Pos() }
func (s *PrintStmt) Pos() token.Pos { return s.X.Pos() }
func (s *VarStmt) Pos() token.Pos   { return s.Name.Pos }

func (s *BadStmt) End() token.Pos   { return s.To }
func (s *ExprStmt) End() token.Pos  { return s.X.End() }
func (s *PrintStmt) End() token.Pos { return s.X.End() }
func (s *VarStmt) End() token.Pos {
	if s.Value != nil {
		return s.Value.End()
	}
	return s.Name.Pos + token.Pos(len(s.Name.Val))
}

// stmtNode() ensures that only statement nodes can be
// assigned to a Stmt.
//
func (*BadStmt) stmtNode()   {}
func (*ExprStmt) stmtNode()  {}
func (*PrintStmt) stmtNode() {}
func (*VarStmt) stmtNode()   {}

// Call the appropriate visitor method on v.
//
// When we add new types of statement, existing uses of StmtAcceptFullVisitor
// will no longer compile until their visitors are updated.  If, instead, you
// want a default visit function to be called, use StmtAcceptPartialVisitor.
func StmtAcceptFullVisitor(e Stmt, v FullStmtVisitor) {
	stmtAcceptVisitor(e, v)
}

// If v implements a visitor for e's type (e.g. PrintStmtVisitor), call the
// appropriate visitor method (e.g. VisitPrintStmt).  Otherwise, call VisitStmt.
func StmtAcceptPartialVisitor(e Stmt, v PartialStmtVisitor) {
	stmtAcceptVisitor(e, v)
}

func stmtAcceptVisitor(e Stmt, v interface{}) {
	switch t := e.(type) {
	case *ExprStmt:
		n, ok := v.(ExprStmtVisitor)
		if ok {
			n.VisitExprStmt(t)
			return
		}
	case *PrintStmt:
		n, ok := v.(PrintStmtVisitor)
		if ok {
			n.VisitPrintStmt(t)
			return
		}
	case *VarStmt:
		n, ok := v.(VarStmtVisitor)
		if ok {
			n.VisitVarStmt(t)
			return
		}
	}
	v.(PartialStmtVisitor).VisitStmt(e)
}

type parser struct {
	l       *lexer
	exprLev int
	indent  int
	trace   bool
	errors  []error
}

func (p *parser) printTrace(a ...interface{}) {
	const dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
	const n = len(dots)
	fmt.Printf("%5d: ", p.cur().Pos)
	i := 2 * p.indent
	for i > n {
		fmt.Print(dots)
		i -= n
	}
	// i <= n
	fmt.Print(dots[0:i])
	fmt.Println(a...)
}

func trace(p *parser, msg string) *parser {
	p.printTrace(msg, "(")
	p.indent++
	return p
}

// Usage pattern: defer un(trace(p, "..."))
func un(p *parser) {
	p.indent--
	p.printTrace(")")
}

func (p *parser) next() {
	p.l.scan()
}

func (p *parser) cur() token.Token {
	return p.l.token()
}

func (p *parser) error(pos token.Pos, msg string) {
	p.errors = append(p.errors, fmt.Errorf("pos %d: %s", pos, msg))
}

func (p *parser) errorExpected(pos token.Pos, msg string) {
	msg = "expected " + msg
	if pos == p.cur().Pos {
		msg += fmt.Sprintf(", found %s", p.cur())
	}
	p.error(pos, msg)
}

func (p *parser) expect(t token.Type) token.Pos {
	pos := p.cur().Pos
	if p.cur().Type != t {
		p.errorExpected(pos, fmt.Sprintf("'%s'", t))
	}
	p.next()
	return pos
}

func (p *parser) advance(to map[token.Type]bool) {
	for ; p.cur().Type != token.EOF; p.next() {
		if to[p.cur().Type] {
			return
		}
	}
}

func (p *parser) parseOperand() Expr {
	if p.trace {
		defer un(trace(p, "Operand"))
	}
	switch p.cur().Type {
	case token.NUMBER, token.STRING, token.FALSE, token.TRUE, token.NIL:
		x := &BasicLit{Value: p.cur()}
		p.next()
		return x
	case token.LPAREN:
		lparen := p.cur().Pos
		p.next()
		p.exprLev++
		x := p.parseExpr()
		p.exprLev--
		rparen := p.expect(token.RPAREN)
		return &ParenExpr{Lparen: lparen, X: x, Rparen: rparen}
	case token.IDENT:
		tok := p.cur()
		p.next()
		return &Ident{Tok: tok}
	}

	pos := p.cur().Pos
	p.errorExpected(pos, "operand")
	p.advance(stmtStart)
	return &BadExpr{From: pos, To: p.cur().Pos}
}

func (p *parser) parsePrimaryExpr() Expr {
	if p.trace {
		defer un(trace(p, "PrimaryExpr"))
	}
	x := p.parseOperand()
	return x
}

func (p *parser) parseUnaryExpr() Expr {
	if p.trace {
		defer un(trace(p, "UnaryExpr"))
	}
	switch p.cur().Type {
	case token.SUB, token.NOT:
		op := p.cur()
		p.next()
		x := p.parseUnaryExpr()
		return &UnaryExpr{Op: op, X: x}
	}
	return p.parsePrimaryExpr()
}

func (p *parser) parseBinaryExpr(prec1 int) Expr {
	if p.trace {
		defer un(trace(p, "BinaryExpr"))
	}
	x := p.parseUnaryExpr()
	for {
		op := p.cur()
		if op.Type.Precedence() < prec1 {
			return x
		}
		p.expect(op.Type)
		y := p.parseBinaryExpr(op.Type.Precedence() + 1)
		x = &BinaryExpr{X: x, Op: op, Y: y}
	}
}

func (p *parser) parseExpr() Expr {
	if p.trace {
		defer un(trace(p, "Expr"))
	}
	return p.parseBinaryExpr(token.LowestPrec + 1)
}

func (p *parser) err() error {
	switch len(p.errors) {
	case 0:
		return nil
	case 1:
		return p.errors[0]
	}
	return fmt.Errorf("%s (and %d more errors)", p.errors[0], len(p.errors)-1)
}

func (p *parser) parseAndReturnExpr() (Expr, error) {
	p.next()
	return p.parseExpr(), p.err()
}

func (p *parser) parsePrintStmt() Stmt {
	if p.trace {
		defer un(trace(p, "PrintStmt"))
	}
	x := p.parseExpr()
	p.expect(token.SEMICOLON)
	return &PrintStmt{X: x}
}

func (p *parser) parseExprStmt() Stmt {
	if p.trace {
		defer un(trace(p, "ExprStmt"))
	}
	x := p.parseExpr()
	p.expect(token.SEMICOLON)
	return &ExprStmt{X: x}
}

func (p *parser) parseStmt() Stmt {
	if p.trace {
		defer un(trace(p, "Statement"))
	}

	switch p.cur().Type {
	case token.PRINT:
		p.next()
		return p.parsePrintStmt()
	default:
		return p.parseExprStmt()
	}
}
func (p *parser) parseDecl() Stmt {
	if p.trace {
		defer un(trace(p, "Decl"))
	}
	if p.cur().Type == token.VAR {
		p.expect(token.VAR)
		n := p.cur()
		p.expect(token.IDENT)
		if n.Type != token.IDENT {
			return &BadStmt{}
		}
		if p.cur().Type == token.ASSIGN {
			p.next()
			return &VarStmt{Name: n, Value: p.parseExpr()}
		}
		return &VarStmt{Name: n, Value: nil}
	}
	return p.parseStmt()
}

func (p *parser) parseProgram() ([]Stmt, error) {
	r := make([]Stmt, 0)
	for p.next(); p.cur().Type != token.EOF; p.next() {
		r = append(r, p.parseDecl())
	}
	return r, p.err()
}

func ParseExpr(x string) (Expr, error) {
	p := &parser{l: lex(x), trace: false}
	return p.parseAndReturnExpr()
}

func ParseProgram(x string) ([]Stmt, error) {
	p := &parser{l: lex(x), trace: false}
	return p.parseProgram()
}

var stmtStart = map[token.Type]bool{
	token.FOR:    true,
	token.IF:     true,
	token.PRINT:  true,
	token.RETURN: true,
	token.VAR:    true,
	token.WHILE:  true,
}
