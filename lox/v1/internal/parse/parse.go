package parse

import "fmt"

type Pos int

type (
	Node interface {
		Pos() Pos // position of the first character belonging to the node
		End() Pos // position of the first character immediately after the node.
	}

	BadExpr struct {
		From, To Pos
	}

	Expr interface {
		Node
	}

	// A BinaryExpr node represents a binary expression.
	BinaryExpr struct {
		X  Expr // left operand
		Op Item // operator
		Y  Expr // right operand
	}

	// A UnaryExpr node represents a unary expression.
	UnaryExpr struct {
		Op Item // operator
		X  Expr // operand
	}

	// A BasicLit node represents a literal of basic type.
	//
	// Note that for itemString items, the literal is stored with its quotes.
	BasicLit struct {
		Value Item // The value
	}

	// A ParenExpr node represents a parenthesized expression.
	ParenExpr struct {
		Lparen Pos // position of "("
		X      Expr
		Rparen Pos // position of ")"
	}

	// An expression visitor visits each piece of an expression.
	ExprVisitor interface {
		VisitBinaryExpr(x *BinaryExpr)
		VisitUnaryExpr(x *UnaryExpr)
		VisitBasicLit(x *BasicLit)
		VisitParenExpr(x *ParenExpr)
	}
)

func ExprAcceptVisitor(e Expr, v ExprVisitor) {
	switch t := e.(type) {
	case *BinaryExpr:
		v.VisitBinaryExpr(t)
	case *UnaryExpr:
		v.VisitUnaryExpr(t)
	case *BasicLit:
		v.VisitBasicLit(t)
	case *ParenExpr:
		v.VisitParenExpr(t)
	}
}

func (x *BadExpr) Pos() Pos    { return x.From }
func (x *BasicLit) Pos() Pos   { return x.Value.Pos }
func (x *BinaryExpr) Pos() Pos { return x.X.Pos() }
func (x *ParenExpr) Pos() Pos  { return x.Lparen }
func (x *UnaryExpr) Pos() Pos  { return x.Op.Pos }

func (x *BadExpr) End() Pos    { return x.To }
func (x *BasicLit) End() Pos   { return x.Value.Pos + Pos(len(x.Value.Val)) }
func (x *BinaryExpr) End() Pos { return x.Y.End() }
func (x *ParenExpr) End() Pos  { return x.Rparen }
func (x *UnaryExpr) End() Pos  { return x.X.End() }

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

func (p *parser) cur() Item {
	return p.l.item()
}

func (p *parser) error(pos Pos, msg string) {
	p.errors = append(p.errors, fmt.Errorf("%d: %s", pos, msg))
}

func (p *parser) errorExpected(pos Pos, msg string) {
	msg = "expected " + msg
	if pos == p.cur().Pos {
		msg += fmt.Sprintf(", found '%#v'", p.cur())
	}
	p.error(pos, msg)
}

func (p *parser) expect(typ ItemType) Pos {
	pos := p.cur().Pos
	if p.cur().Type != typ {
		p.errorExpected(pos, fmt.Sprintf("'%v'", typ))
	}
	p.next()
	return pos
}

func (p *parser) advance(to map[ItemType]bool) {
	for ; p.cur().Type != ItemEOF; p.next() {
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
	case ItemNumber, ItemString, ItemFalse, ItemTrue, ItemNil:
		x := &BasicLit{Value: p.cur()}
		p.next()
		return x
	case ItemLeftParen:
		lparen := p.cur().Pos
		p.next()
		p.exprLev++
		x := p.parseExpr()
		p.exprLev--
		rparen := p.expect(ItemRightParen)
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
	case ItemMinus, ItemBang:
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
		if op.Type.precedence() < prec1 {
			return x
		}
		p.expect(op.Type)
		y := p.parseBinaryExpr(op.Type.precedence() + 1)
		x = &BinaryExpr{X: x, Op: op, Y: y}
	}
}

func (p *parser) parseExpr() Expr {
	if p.trace {
		defer un(trace(p, "Expr"))
	}
	return p.parseBinaryExpr(lowestPrec + 1)
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

var stmtStart = map[ItemType]bool{
	ItemFor:    true,
	ItemIf:     true,
	ItemPrint:  true,
	ItemReturn: true,
	ItemVar:    true,
	ItemWhile:  true,
}
