package ast

import "github.com/alan-strohm/misc/lox/v1/internal/token"

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

	// A CallExpr node represents an expression followed by an argument list.
	CallExpr struct {
		Fun    Expr      // function expression
		Lparen token.Pos // position of "("
		Args   []Expr    // function arguments; or nil
		Rparen token.Pos // position of ")"
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

	// An Assign node represents a variable assignment expression
	//
	Assign struct {
		Name  token.Token
		Value Expr
	}

	// A FullExprVisitor implements visit methods for each concrete type of expression.
	FullExprVisitor interface {
		VisitIdent(x *Ident)
		VisitBinaryExpr(x *BinaryExpr)
		VisitUnaryExpr(x *UnaryExpr)
		VisitCallExpr(x *CallExpr)
		VisitBasicLit(x *BasicLit)
		VisitParenExpr(x *ParenExpr)
		VisitAssign(x *Assign)
	}

	// A DefaultExprVisitor implements FullExprVisitor, by calling a generic default function.
	// Clients can embed this struct if they don't want to implement each method
	// of FullExprVisitor.
	DefaultExprVisitor struct {
		def func(e Expr)
	}

	// An ExprWalker implements FullExprVisitor by walking the expression tree.
	// Clients can embed this struct and then provide their own implementation of
	// whichever visits they wish.
	ExprWalker struct {
		parent FullExprVisitor
	}
)

// Create an expression visitor which will call def for every type of
// expression.
func NewDefaultExpr(def func(e Expr)) *DefaultExprVisitor {
	return &DefaultExprVisitor{def: def}
}

// Create an expression walker which will walk an expression tree, calling
// parent on each sub-expression.
func NewExprWalker(parent FullExprVisitor) *ExprWalker {
	return &ExprWalker{parent: parent}
}

func (d *DefaultExprVisitor) VisitIdent(x *Ident)           { d.def(x) }
func (w *ExprWalker) VisitIdent(x *Ident)                   {}
func (d *DefaultExprVisitor) VisitBinaryExpr(x *BinaryExpr) { d.def(x) }
func (w *ExprWalker) VisitBinaryExpr(x *BinaryExpr) {
	ExprAcceptVisitor(x.X, w.parent)
	ExprAcceptVisitor(x.Y, w.parent)
}
func (d *DefaultExprVisitor) VisitUnaryExpr(x *UnaryExpr) { d.def(x) }
func (w *ExprWalker) VisitUnaryExpr(x *UnaryExpr)         { ExprAcceptVisitor(x.X, w.parent) }
func (d *DefaultExprVisitor) VisitCallExpr(x *CallExpr)   { d.def(x) }
func (w *ExprWalker) VisitCallExpr(x *CallExpr) {
	ExprAcceptVisitor(x.Fun, w.parent)
	for _, arg := range x.Args {
		ExprAcceptVisitor(arg, w.parent)
	}
}
func (d *DefaultExprVisitor) VisitBasicLit(x *BasicLit)   { d.def(x) }
func (w *ExprWalker) VisitBasicLit(x *BasicLit)           {}
func (d *DefaultExprVisitor) VisitParenExpr(x *ParenExpr) { d.def(x) }
func (w *ExprWalker) VisitParenExpr(x *ParenExpr)         { ExprAcceptVisitor(x.X, w.parent) }
func (d *DefaultExprVisitor) VisitAssign(x *Assign)       { d.def(x) }
func (w *ExprWalker) VisitAssign(x *Assign)               { ExprAcceptVisitor(x.Value, w.parent) }

// Call the appropriate visitor method on v.
func ExprAcceptVisitor(e Expr, v FullExprVisitor) {
	switch t := e.(type) {
	case *Ident:
		v.VisitIdent(t)
	case *BinaryExpr:
		v.VisitBinaryExpr(t)
	case *UnaryExpr:
		v.VisitUnaryExpr(t)
	case *CallExpr:
		v.VisitCallExpr(t)
	case *BasicLit:
		v.VisitBasicLit(t)
	case *ParenExpr:
		v.VisitParenExpr(t)
	case *Assign:
		v.VisitAssign(t)
	}
}

func (x *BadExpr) Pos() token.Pos    { return x.From }
func (x *Ident) Pos() token.Pos      { return x.Tok.Pos }
func (x *BasicLit) Pos() token.Pos   { return x.Value.Pos }
func (x *BinaryExpr) Pos() token.Pos { return x.X.Pos() }
func (x *ParenExpr) Pos() token.Pos  { return x.Lparen }
func (x *UnaryExpr) Pos() token.Pos  { return x.Op.Pos }
func (x *CallExpr) Pos() token.Pos   { return x.Fun.Pos() }
func (x *Assign) Pos() token.Pos     { return x.Name.Pos }

func (x *BadExpr) End() token.Pos    { return x.To }
func (x *Ident) End() token.Pos      { return x.Tok.Pos + token.Pos(len(x.Tok.Val)) }
func (x *BasicLit) End() token.Pos   { return x.Value.Pos + token.Pos(len(x.Value.Val)) }
func (x *BinaryExpr) End() token.Pos { return x.Y.End() }
func (x *ParenExpr) End() token.Pos  { return x.Rparen }
func (x *UnaryExpr) End() token.Pos  { return x.X.End() }
func (x *CallExpr) End() token.Pos   { return x.Rparen + 1 }
func (x *Assign) End() token.Pos     { return x.Value.End() }

// exprNode() ensures that only expression/type nodes can be
// assigned to an Expr.
//
func (*BadExpr) exprNode()    {}
func (*Ident) exprNode()      {}
func (*BasicLit) exprNode()   {}
func (*ParenExpr) exprNode()  {}
func (*UnaryExpr) exprNode()  {}
func (*CallExpr) exprNode()   {}
func (*BinaryExpr) exprNode() {}
func (*Assign) exprNode()     {}

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
		X         Expr      // expression
		Semicolon token.Pos // Position of the semicolon
	}

	// A PrintStmt node represents a print statement.
	//
	PrintStmt struct {
		Print     token.Pos // Position of the "print" keyword.
		X         Expr      // expression
		Semicolon token.Pos // Position of the semicolon
	}

	// A VarDecl node represents a var statement.
	//
	VarDecl struct {
		Var       token.Pos // Position of the "var" keyword.
		Name      token.Token
		Value     Expr      // initial value; or nil
		Semicolon token.Pos // Position of the semicolon
	}

	// A FuncDecl node represents a function declaration.
	FunDecl struct {
		Fun    token.Pos     // Position of the "fun" keyword.
		Name   token.Token   // function/method name
		Params []token.Token // function parameters
		Body   Stmt          // function body *BlockStmt
	}

	// A BlockStmt node represents a braced statement list.
	BlockStmt struct {
		Lbrace token.Pos // position of "{"
		List   []Stmt
		Rbrace token.Pos // position of "}", if any (may be absent due to syntax error)
	}

	// An IfStmt node represents an if statement.
	IfStmt struct {
		If   token.Pos // position of "if" keyword
		Cond Expr      // condition
		Body Stmt
		Else Stmt // else branch; or nil
	}

	// A WhileStmt node represents a while statement.
	WhileStmt struct {
		While token.Pos // position of "while" keyword
		Cond  Expr      // condition
		Body  Stmt
	}

	// A ForStmt node represents a for statement.
	ForStmt struct {
		For  token.Pos // position of "for" keyword
		Init Stmt      // VarDecl or ExprStmt or nil
		Cond Expr      // condition or nil
		Post Expr      // increment expression or nil
		Body Stmt
	}

	// A ReturnStmt node represents a return statement.
	ReturnStmt struct {
		Return token.Pos // position of "return" keyword
		Result Expr      // result expressions; or nil
	}

	FullStmtVisitor interface {
		VisitExprStmt(x *ExprStmt)
		VisitPrintStmt(x *PrintStmt)
		VisitVarDecl(x *VarDecl)
		VisitFunDecl(x *FunDecl)
		VisitBlockStmt(x *BlockStmt)
		VisitIfStmt(x *IfStmt)
		VisitWhileStmt(x *WhileStmt)
		VisitForStmt(x *ForStmt)
		VisitReturnStmt(x *ReturnStmt)
	}

	// A DefaultStmtVisitor implements FullStmtVisitor, by calling a generic default function.
	// Clients can embed this struct if they don't want to implement each method
	// of FullStmtVisitor.
	DefaultStmtVisitor struct {
		def func(e Stmt)
	}

	// A FullVisitor can visit any node in the AST.
	FullVisitor interface {
		FullExprVisitor
		FullStmtVisitor
	}

	// A Walker implements FullAstWalker by walking the syntax tree.
	// Clients can embed this struct and then provide their own implementation of
	// whichever visits they wish.
	Walker struct {
		*ExprWalker
		parent FullVisitor
	}
)

// Create a statement visitor which will call def for every type of statement.
func NewDefaultStmt(def func(e Stmt)) *DefaultStmtVisitor {
	return &DefaultStmtVisitor{def: def}
}

// Create an expression walker which will walk an expression tree, calling
// parent on each sub-expression.
func NewWalker(parent FullVisitor) *Walker {
	return &Walker{parent: parent, ExprWalker: NewExprWalker(parent)}
}

func (d *DefaultStmtVisitor) VisitExprStmt(s *ExprStmt)   { d.def(s) }
func (w *Walker) VisitExprStmt(s *ExprStmt)               { ExprAcceptVisitor(s.X, w.parent) }
func (d *DefaultStmtVisitor) VisitPrintStmt(s *PrintStmt) { d.def(s) }
func (w *Walker) VisitPrintStmt(s *PrintStmt)             { ExprAcceptVisitor(s.X, w.parent) }
func (d *DefaultStmtVisitor) VisitVarDecl(s *VarDecl)     { d.def(s) }
func (w *Walker) VisitVarDecl(s *VarDecl)                 { ExprAcceptVisitor(s.Value, w.parent) }
func (d *DefaultStmtVisitor) VisitFunDecl(s *FunDecl)     { d.def(s) }
func (w *Walker) VisitFunDecl(s *FunDecl)                 { StmtAcceptVisitor(s.Body, w.parent) }
func (d *DefaultStmtVisitor) VisitBlockStmt(s *BlockStmt) { d.def(s) }
func (w *Walker) VisitBlockStmt(s *BlockStmt) {
	for _, s := range s.List {
		StmtAcceptVisitor(s, w.parent)
	}
}
func (d *DefaultStmtVisitor) VisitIfStmt(s *IfStmt) { d.def(s) }
func (w *Walker) VisitIfStmt(s *IfStmt) {
	ExprAcceptVisitor(s.Cond, w.parent)
	StmtAcceptVisitor(s.Body, w.parent)
	if s.Else != nil {
		StmtAcceptVisitor(s.Else, w.parent)
	}
}
func (d *DefaultStmtVisitor) VisitWhileStmt(s *WhileStmt) { d.def(s) }
func (w *Walker) VisitWhileStmt(s *WhileStmt) {
	ExprAcceptVisitor(s.Cond, w.parent)
	StmtAcceptVisitor(s.Body, w.parent)
}
func (d *DefaultStmtVisitor) VisitForStmt(s *ForStmt) { d.def(s) }
func (w *Walker) VisitForStmt(s *ForStmt) {
	if s.Init != nil {
		StmtAcceptVisitor(s.Init, w.parent)
	}
	if s.Cond != nil {
		ExprAcceptVisitor(s.Cond, w.parent)
	}
	if s.Post != nil {
		ExprAcceptVisitor(s.Post, w.parent)
	}
	StmtAcceptVisitor(s.Body, w.parent)
}
func (d *DefaultStmtVisitor) VisitReturnStmt(s *ReturnStmt) { d.def(s) }
func (w *Walker) VisitReturnStmt(s *ReturnStmt)             { ExprAcceptVisitor(s.Result, w.parent) }

// Pos and End implementations for statement nodes.

func (s *BadStmt) Pos() token.Pos    { return s.From }
func (s *ExprStmt) Pos() token.Pos   { return s.X.Pos() }
func (s *PrintStmt) Pos() token.Pos  { return s.Print }
func (s *VarDecl) Pos() token.Pos    { return s.Var }
func (s *FunDecl) Pos() token.Pos    { return s.Fun }
func (s *BlockStmt) Pos() token.Pos  { return s.Lbrace }
func (s *IfStmt) Pos() token.Pos     { return s.If }
func (s *WhileStmt) Pos() token.Pos  { return s.While }
func (s *ForStmt) Pos() token.Pos    { return s.For }
func (s *ReturnStmt) Pos() token.Pos { return s.Return }

func (s *BadStmt) End() token.Pos   { return s.To }
func (s *ExprStmt) End() token.Pos  { return s.Semicolon + 1 }
func (s *PrintStmt) End() token.Pos { return s.Semicolon + 1 }
func (s *VarDecl) End() token.Pos   { return s.Semicolon + 1 }
func (s *FunDecl) End() token.Pos   { return s.Body.End() }
func (s *BlockStmt) End() token.Pos {
	if s.Rbrace.IsValid() {
		return s.Rbrace + 1
	}
	if n := len(s.List); n > 0 {
		return s.List[n-1].End()
	}
	return s.Lbrace + 1
}
func (s *IfStmt) End() token.Pos {
	if s.Else != nil {
		return s.Else.End()
	}
	return s.Body.End()
}
func (s *WhileStmt) End() token.Pos  { return s.Body.End() }
func (s *ForStmt) End() token.Pos    { return s.Body.End() }
func (s *ReturnStmt) End() token.Pos { return s.Result.End() }

// stmtNode() ensures that only statement nodes can be
// assigned to a Stmt.
//
func (*BadStmt) stmtNode()    {}
func (*ExprStmt) stmtNode()   {}
func (*PrintStmt) stmtNode()  {}
func (*VarDecl) stmtNode()    {}
func (*FunDecl) stmtNode()    {}
func (*BlockStmt) stmtNode()  {}
func (*IfStmt) stmtNode()     {}
func (*WhileStmt) stmtNode()  {}
func (*ForStmt) stmtNode()    {}
func (*ReturnStmt) stmtNode() {}

// Call the appropriate visitor method on v.
func StmtAcceptVisitor(e Stmt, v FullStmtVisitor) {
	switch t := e.(type) {
	case *ExprStmt:
		v.VisitExprStmt(t)
	case *PrintStmt:
		v.VisitPrintStmt(t)
	case *VarDecl:
		v.VisitVarDecl(t)
	case *FunDecl:
		v.VisitFunDecl(t)
	case *BlockStmt:
		v.VisitBlockStmt(t)
	case *IfStmt:
		v.VisitIfStmt(t)
	case *WhileStmt:
		v.VisitWhileStmt(t)
	case *ForStmt:
		v.VisitForStmt(t)
	case *ReturnStmt:
		v.VisitReturnStmt(t)
	}
}
