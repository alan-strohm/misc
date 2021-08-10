package parse

import (
	"fmt"

	"github.com/alan-strohm/misc/lox/v1/internal/ast"
	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

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

func (p *parser) parseOperand() ast.Expr {
	if p.trace {
		defer un(trace(p, "Operand"))
	}
	switch p.cur().Type {
	case token.NUMBER, token.STRING, token.FALSE, token.TRUE, token.NIL:
		x := &ast.BasicLit{Value: p.cur()}
		p.next()
		return x
	case token.LPAREN:
		lparen := p.cur().Pos
		p.next()
		p.exprLev++
		x := p.parseExpr()
		p.exprLev--
		rparen := p.expect(token.RPAREN)
		return &ast.ParenExpr{Lparen: lparen, X: x, Rparen: rparen}
	case token.IDENT:
		tok := p.cur()
		p.next()
		return &ast.Ident{Tok: tok}
	}

	pos := p.cur().Pos
	p.errorExpected(pos, "operand")
	p.advance(stmtStart)
	return &ast.BadExpr{From: pos, To: p.cur().Pos}
}

func (p *parser) parseCall(fun ast.Expr) ast.Expr {
	if p.trace {
		defer un(trace(p, "Call"))
	}
	lparen := p.expect(token.LPAREN)
	p.exprLev++
	var list []ast.Expr
	for p.cur().Type != token.RPAREN && p.cur().Type != token.EOF {
		if len(list) >= 255 {
			p.error(p.cur().Pos, "more than 255 arguments")
		}
		list = append(list, p.parseExpr())
		if p.cur().Type != token.COMMA {
			break
		}
		p.next()
	}
	p.exprLev--
	rparen := p.expect(token.RPAREN)
	return &ast.CallExpr{Fun: fun, Lparen: lparen, Args: list, Rparen: rparen}
}

func (p *parser) parsePrimaryExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "PrimaryExpr"))
	}
	x := p.parseOperand()
L:
	for {
		switch p.cur().Type {
		case token.LPAREN:
			x = p.parseCall(x)
		default:
			break L
		}
	}
	return x
}

func (p *parser) parseUnaryExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "UnaryExpr"))
	}
	switch p.cur().Type {
	case token.SUB, token.NOT:
		op := p.cur()
		p.next()
		x := p.parseUnaryExpr()
		return &ast.UnaryExpr{Op: op, X: x}
	}
	return p.parsePrimaryExpr()
}

func (p *parser) parseBinaryExpr(prec1 int) ast.Expr {
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
		x = &ast.BinaryExpr{X: x, Op: op, Y: y}
	}
}

func (p *parser) parseAssign() ast.Expr {
	x := p.parseBinaryExpr(token.LowestPrec + 1)
	if p.cur().Type == token.ASSIGN {
		p.expect(token.ASSIGN)
		y := p.parseAssign()
		if lhs, ok := x.(*ast.Ident); ok {
			return &ast.Assign{Name: lhs.Tok, Value: y}
		} else {
			p.errorExpected(x.Pos(), "valid lhs expression")
			return &ast.BadExpr{From: x.Pos(), To: y.End()}
		}
	}
	return x
}

func (p *parser) parseExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "Expr"))
	}
	return p.parseAssign()
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

func (p *parser) parseAndReturnExpr() (ast.Expr, error) {
	p.next()
	return p.parseExpr(), p.err()
}

// Parse a print statement.
func (p *parser) parsePrintStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "PrintStmt"))
	}
	pos := p.expect(token.PRINT)
	x := p.parseExpr()
	end := p.expect(token.SEMICOLON)
	return &ast.PrintStmt{Print: pos, X: x, Semicolon: end}
}

func (p *parser) parseExprStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "ExprStmt"))
	}
	x := p.parseExpr()
	end := p.expect(token.SEMICOLON)
	return &ast.ExprStmt{X: x, Semicolon: end}
}

// ----------------------------------------------------------------------------
// Blocks

func (p *parser) parseStmtList() (list []ast.Stmt) {
	if p.trace {
		defer un(trace(p, "StatementList"))
	}

	for p.cur().Type != token.EOF && p.cur().Type != token.RBRACE {
		list = append(list, p.parseDecl())
	}
	return
}

func (p *parser) parseBlockStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "BlockStmt"))
	}

	lbrace := p.expect(token.LBRACE)
	r := p.parseStmtList()
	rbrace := p.expect(token.RBRACE)
	return &ast.BlockStmt{Lbrace: lbrace, List: r, Rbrace: rbrace}
}

func (p *parser) parseCond() ast.Expr {
	p.expect(token.LPAREN)
	cond := p.parseExpr()
	p.expect(token.RPAREN)
	return cond
}

func (p *parser) parseWhileStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "WhileStmt"))
	}
	pos := p.expect(token.WHILE)
	cond := p.parseCond()
	body := p.parseStmt()
	return &ast.WhileStmt{While: pos, Cond: cond, Body: body}
}

func (p *parser) parseIfStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "IfStmt"))
	}
	pos := p.expect(token.IF)
	cond := p.parseCond()
	body := p.parseStmt()
	var else_ ast.Stmt
	if p.cur().Type == token.ELSE {
		p.expect(token.ELSE)
		else_ = p.parseStmt()
	}
	return &ast.IfStmt{If: pos, Cond: cond, Body: body, Else: else_}
}

func (p *parser) parseStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "Statement"))
	}

	switch p.cur().Type {
	case token.PRINT:
		return p.parsePrintStmt()
	case token.LBRACE:
		return p.parseBlockStmt()
	case token.IF:
		return p.parseIfStmt()
	case token.WHILE:
		return p.parseWhileStmt()
	case token.FOR:
		return p.parseForStmt()
	case token.RETURN:
		return p.parseReturnStmt()
	default:
		return p.parseExprStmt()
	}
}

func (p *parser) parseVar() ast.Stmt {
	if p.trace {
		defer un(trace(p, "Var"))
	}
	pos := p.expect(token.VAR)
	n := p.cur()
	p.expect(token.IDENT)
	var e ast.Expr
	if p.cur().Type == token.ASSIGN {
		p.next()
		e = p.parseExpr()
	}
	end := p.expect(token.SEMICOLON)
	return &ast.VarDecl{Var: pos, Name: n, Value: e, Semicolon: end}
}

func (p *parser) parseFun() ast.Stmt {
	if p.trace {
		defer un(trace(p, "Fun"))
	}
	pos := p.expect(token.FUN)
	n := p.cur()
	p.expect(token.IDENT)
	p.expect(token.LPAREN)
	var params []token.Token
	for p.cur().Type != token.RPAREN {
		if len(params) >= 255 {
			p.error(p.cur().Pos, "can't have more than 255 arguments")
		}
		if len(params) > 0 {
			if p.cur().Type != token.COMMA {
				p.expect(token.COMMA)
				break
			}
			p.expect(token.COMMA)
		}
		params = append(params, p.cur())
		p.next()
	}
	p.expect(token.RPAREN)
	return &ast.FunDecl{Fun: pos, Name: n, Params: params, Body: p.parseBlockStmt()}
}

func (p *parser) parseDecl() ast.Stmt {
	if p.trace {
		defer un(trace(p, "Decl"))
	}
	switch p.cur().Type {
	case token.VAR:
		return p.parseVar()
	case token.FUN:
		return p.parseFun()
	default:
		return p.parseStmt()
	}
}

func (p *parser) parseForStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "ForStmt"))
	}
	pos := p.expect(token.FOR)
	p.expect(token.LPAREN)

	var init ast.Stmt
	if p.cur().Type == token.SEMICOLON {
		p.expect(token.SEMICOLON)
	} else if p.cur().Type == token.VAR {
		init = p.parseDecl()
	} else {
		init = p.parseExprStmt()
	}

	var cond ast.Expr
	if p.cur().Type != token.SEMICOLON {
		cond = p.parseExpr()
	}
	p.expect(token.SEMICOLON)

	var post ast.Expr
	if p.cur().Type != token.RPAREN {
		post = p.parseExpr()
	}
	p.expect(token.RPAREN)
	body := p.parseStmt()
	return &ast.ForStmt{For: pos, Init: init, Cond: cond, Post: post, Body: body}
}

func (p *parser) parseReturnStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "ReturnStmt"))
	}
	pos := p.expect(token.RETURN)
	var x ast.Expr
	if p.cur().Type != token.SEMICOLON {
		x = p.parseExpr()
	}
	p.expect(token.SEMICOLON)
	return &ast.ReturnStmt{Return: pos, Result: x}
}

func (p *parser) parseProgram() ([]ast.Stmt, error) {
	return p.parseStmtList(), p.err()
}

func ParseExpr(x string) (ast.Expr, error) {
	p := &parser{l: lex(x), trace: false}
	return p.parseAndReturnExpr()
}

func ParseProgram(x string) ([]ast.Stmt, error) {
	p := &parser{l: lex(x), trace: false}
	p.next()
	return p.parseProgram()
}

var stmtStart = map[token.Type]bool{
	token.FOR:    true,
	token.IF:     true,
	token.PRINT:  true,
	token.RETURN: true,
	token.VAR:    true,
	token.FUN:    true,
	token.WHILE:  true,
}
