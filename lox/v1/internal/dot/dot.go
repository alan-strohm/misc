package dot

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/alan-strohm/misc/lox/v1/internal/ast"
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

func (p *printer) addNode(name string) string {
	n := p.pushIdent()
	p.lines = append(p.lines, fmt.Sprintf("%s [label=%s]", n, strconv.Quote(name)))
	return n
}

func (p *printer) addEdge(from, to string) {
	p.lines = append(p.lines, fmt.Sprintf("%s -> %s", from, to))
}
func (p *printer) addEdgeToExpr(from string, x ast.Expr) {
	ast.ExprAcceptFullVisitor(x, p)
	p.addEdge(from, p.popIdent())
}
func (p *printer) addEdgeToStmt(from string, x ast.Stmt) {
	ast.StmtAcceptFullVisitor(x, p)
	p.addEdge(from, p.popIdent())
}

func (p *printer) VisitBinaryExpr(e *ast.BinaryExpr) {
	this := p.addNode(e.Op.Val)
	p.addEdgeToExpr(this, e.X)
	p.addEdgeToExpr(this, e.Y)
}
func (p *printer) VisitCallExpr(e *ast.CallExpr) {
	this := p.addNode("call")
	p.addEdgeToExpr(this, e.Fun)
	for _, arg := range e.Args {
		p.addEdgeToExpr(this, arg)
	}
}
func (p *printer) VisitUnaryExpr(e *ast.UnaryExpr) {
	this := p.addNode(e.Op.Val)
	p.addEdgeToExpr(this, e.X)
}
func (p *printer) VisitBasicLit(e *ast.BasicLit) {
	p.addNode(e.Value.Val)
}
func (p *printer) VisitIdent(e *ast.Ident) {
	p.addNode(e.Tok.Val)
}
func (p *printer) VisitParenExpr(e *ast.ParenExpr) {
	this := p.addNode("()")
	p.addEdgeToExpr(this, e.X)
}
func (p *printer) VisitAssign(e *ast.Assign) {
	this := p.addNode(e.Name.Val)
	assign := p.addNode("=")
	p.addEdge(this, p.popIdent())
	p.addEdgeToExpr(assign, e.Value)
}
func (p *printer) VisitExprStmt(x *ast.ExprStmt) {
	this := p.addNode("Expr")
	p.addEdgeToExpr(this, x.X)
}
func (p *printer) VisitPrintStmt(x *ast.PrintStmt) {
	this := p.addNode("print")
	p.addEdgeToExpr(this, x.X)
}
func (p *printer) VisitVarDecl(x *ast.VarDecl) {
	this := p.addNode("var " + x.Name.Val)
	if x.Value != nil {
		p.addEdgeToExpr(this, x.Value)
	}
}
func (p *printer) VisitFunDecl(x *ast.FunDecl) {
	this := p.addNode("fun " + x.Name.Val)
	p.addEdgeToStmt(this, x.Body)
}
func (p *printer) VisitBlockStmt(x *ast.BlockStmt) {
	this := p.addNode("{}")
	for _, s := range x.List {
		p.addEdgeToStmt(this, s)
	}
}
func (p *printer) VisitIfStmt(x *ast.IfStmt) {
	this := p.addNode("if")
	p.addEdgeToExpr(this, x.Cond)
	p.addEdgeToStmt(this, x.Body)
	if x.Else != nil {
		p.addEdgeToStmt(this, x.Else)
	}
}
func (p *printer) VisitWhileStmt(x *ast.WhileStmt) {
	this := p.addNode("while")
	p.addEdgeToExpr(this, x.Cond)
	p.addEdgeToStmt(this, x.Body)
}
func (p *printer) VisitForStmt(x *ast.ForStmt) {
	this := p.addNode("for")
	if x.Init != nil {
		p.addEdgeToStmt(this, x.Init)
	}
	if x.Cond != nil {
		p.addEdgeToExpr(this, x.Cond)
	}
	if x.Post != nil {
		p.addEdgeToExpr(this, x.Post)
	}
	p.addEdgeToStmt(this, x.Body)
}

func (p *printer) VisitReturnStmt(x *ast.ReturnStmt) {
	this := p.addNode("return")
	p.addEdgeToExpr(this, x.Result)
}

func ExprToDot(e ast.Expr) string {
	p := &printer{}
	ast.ExprAcceptFullVisitor(e, p)
	return fmt.Sprintf("digraph G {\n  %s\n}", strings.Join(p.lines, "\n  "))
}

func ProgToDot(prog []ast.Stmt) string {
	p := &printer{}
	root := p.addNode("root")
	for _, s := range prog {
		p.addEdgeToStmt(root, s)
	}
	return fmt.Sprintf("digraph G {\n  %s\n}", strings.Join(p.lines, "\n  "))
}
