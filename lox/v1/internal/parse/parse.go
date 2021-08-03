package parse

import (
	"fmt"

	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

type (
	Node interface {
		Pos() token.Pos // position of the first character belonging to the node
		End() token.Pos // position of the first character immediately after the node.
	}

	BadExpr struct {
		From, To token.Pos
	}

	Expr interface {
		Node
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
		BinaryExprVisitor
		UnaryExprVisitor
		BasicLitVisitor
		ParenExprVisitor
	}
	PartialExprVisitor interface {
		VisitExpr(x Expr)
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
	case *BinaryExpr:
		n, ok := v.(BinaryExprVisitor)
		if ok {
			n.VisitBinaryExpr(t)
		}
		return
	case *UnaryExpr:
		n, ok := v.(UnaryExprVisitor)
		if ok {
			n.VisitUnaryExpr(t)
		}
		return
	case *BasicLit:
		n, ok := v.(BasicLitVisitor)
		if ok {
			n.VisitBasicLit(t)
		}
		return
	case *ParenExpr:
		n, ok := v.(ParenExprVisitor)
		if ok {
			n.VisitParenExpr(t)
		}
		return
	}
	v.(PartialExprVisitor).VisitExpr(e)
}

func (x *BadExpr) Pos() token.Pos    { return x.From }
func (x *BasicLit) Pos() token.Pos   { return x.Value.Pos }
func (x *BinaryExpr) Pos() token.Pos { return x.X.Pos() }
func (x *ParenExpr) Pos() token.Pos  { return x.Lparen }
func (x *UnaryExpr) Pos() token.Pos  { return x.Op.Pos }

func (x *BadExpr) End() token.Pos    { return x.To }
func (x *BasicLit) End() token.Pos   { return x.Value.Pos + token.Pos(len(x.Value.Val)) }
func (x *BinaryExpr) End() token.Pos { return x.Y.End() }
func (x *ParenExpr) End() token.Pos  { return x.Rparen }
func (x *UnaryExpr) End() token.Pos  { return x.X.End() }

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
		msg += fmt.Sprintf(", found '%s'", p.cur())
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

func (p *parser) Parse() (Expr, error) {
	p.next()
	exp := p.parseExpr()
	switch len(p.errors) {
	case 0:
		return exp, nil
	case 1:
		return nil, p.errors[0]
	}
	return nil, fmt.Errorf("%s (and %d more errors)", p.errors[0], len(p.errors)-1)
}

func ParseExpr(x string) (Expr, error) {
	p := &parser{l: lex(x), trace: false}
	return p.Parse()
}

var stmtStart = map[token.Type]bool{
	token.FOR:    true,
	token.IF:     true,
	token.PRINT:  true,
	token.RETURN: true,
	token.VAR:    true,
	token.WHILE:  true,
}
