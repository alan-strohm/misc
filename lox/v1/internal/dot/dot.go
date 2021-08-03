package dot

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/alan-strohm/misc/lox/v1/internal/parse"
)

type exprPrinter struct {
	numNodes int
	path     []string
	lines    []string
}

func (p *exprPrinter) popIdent() string {
	r := p.path[len(p.path)-1]
	p.path = p.path[0 : len(p.path)-1]
	return r
}
func (p *exprPrinter) pushIdent() string {
	p.numNodes++
	id := fmt.Sprintf("id%05d", p.numNodes)
	p.path = append(p.path, id)
	return id
}

func (p *exprPrinter) VisitBinaryExpr(e *parse.BinaryExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	parse.ExprAcceptFullVisitor(e.Y, p)
	y := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="%s"]`, this, e.Op.Val))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, y))
}
func (p *exprPrinter) VisitUnaryExpr(e *parse.UnaryExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="%s"]`, this, e.Op.Val))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
}
func (p *exprPrinter) VisitBasicLit(e *parse.BasicLit) {
	this := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label=%s]`, this, strconv.Quote(e.Value.Val)))
}
func (p *exprPrinter) VisitParenExpr(e *parse.ParenExpr) {
	this := p.pushIdent()
	parse.ExprAcceptFullVisitor(e.X, p)
	x := p.popIdent()
	p.lines = append(p.lines, fmt.Sprintf(`%s [label="()"]`, this))
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", this, x))
}

func ExprToDot(e parse.Expr) string {
	p := &exprPrinter{}
	parse.ExprAcceptFullVisitor(e, p)
	return fmt.Sprintf("digraph G {\n  %s\n}", strings.Join(p.lines, "\n  "))
}
