package dot

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/alan-strohm/misc/lox/v1/internal/parse"
)

type printer struct {
	numNodes int
	path     []string
	lines    []string
}

func (p *printer) popIdent() string {
	r := p.path[len(p.path)-1]
	p.path = p.path[0 : len(p.path)-1]
	return r
}
func (p *printer) pushIdent() string {
	p.numNodes++
	id := fmt.Sprintf("id%05d", p.numNodes)
	p.path = append(p.path, id)
	return id
}

func (p *printer) VisitBinaryExpr(e *parse.BinaryExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	parse.ExprAcceptFullVisitor(e.Y, p)
	y := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="%s"]`, this, e.Op.Val))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, y))
}
func (p *printer) VisitUnaryExpr(e *parse.UnaryExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="%s"]`, this, e.Op.Val))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
}
func (p *printer) VisitBasicLit(e *parse.BasicLit) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label=%s]`, this, strconv.Quote(e.Value.Val)))
}
func (p *printer) VisitIdent(e *parse.Ident) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label=%s]`, this, e.Tok.Val))
}
func (p *printer) VisitParenExpr(e *parse.ParenExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="()"]`, this))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
}
func (p *printer) VisitAssign(e *parse.Assign) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="%s"]`, this, e.Name.Val))
	assign := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="="]`, assign))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	parse.ExprAcceptFullVisitor(e.Value, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", assign, p.popIdent()))
}
func (p *printer) VisitExprStmt(x *parse.ExprStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="Expr"]`, this))
	parse.ExprAcceptFullVisitor(x.X, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
}
func (p *printer) VisitPrintStmt(x *parse.PrintStmt) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(x.X, p)
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="print"]`, this))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
}
func (p *printer) VisitVarStmt(x *parse.VarStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="var %s"]`, this, x.Name.Val))
	if x.Value != nil {
		parse.ExprAcceptFullVisitor(x.Value, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
}
func (p *printer) VisitBlockStmt(x *parse.BlockStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="{}"]`, this))
	for _, s := range x.List {
		parse.StmtAcceptFullVisitor(s, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
}
func (p *printer) VisitIfStmt(x *parse.IfStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="if"]`, this))
	parse.ExprAcceptFullVisitor(x.Cond, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	parse.StmtAcceptFullVisitor(x.Body, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	if x.Else != nil {
		parse.StmtAcceptFullVisitor(x.Else, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
}
func (p *printer) VisitWhileStmt(x *parse.WhileStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="while"]`, this))
	parse.ExprAcceptFullVisitor(x.Cond, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	parse.StmtAcceptFullVisitor(x.Body, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
}
func (p *printer) VisitForStmt(x *parse.ForStmt) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="for"]`, this))
	if x.Init != nil {
		parse.StmtAcceptFullVisitor(x.Init, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
	if x.Cond != nil {
		parse.ExprAcceptFullVisitor(x.Cond, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
	if x.Post != nil {
		parse.ExprAcceptFullVisitor(x.Post, p)
		p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
	}
	parse.StmtAcceptFullVisitor(x.Body, p)
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, p.popIdent()))
}

func ExprToDot(e parse.Expr) string {
	p := &printer{}
	parse.ExprAcceptFullVisitor(e, p)
	return fmt.Sprintf("digraph G {\n  %s\n}", strings.Join(p.lines, "\n  "))
}

func ProgToDot(prog []parse.Stmt) string {
	p := &printer{}
	for _, s := range prog {
		parse.StmtAcceptFullVisitor(s, p)
	}
	return fmt.Sprintf("digraph G {\n  %s\n}", strings.Join(p.lines, "\n  "))
}
