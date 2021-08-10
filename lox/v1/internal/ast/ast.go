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
	CallExprVisitor interface {
		VisitCallExpr(x *CallExpr)
	}
	BasicLitVisitor interface {
		VisitBasicLit(x *BasicLit)
	}
	ParenExprVisitor interface {
		VisitParenExpr(x *ParenExpr)
	}
	AssignVisitor interface {
		VisitAssign(x *Assign)
	}
	FullExprVisitor interface {
		IdentVisitor
		BinaryExprVisitor
		UnaryExprVisitor
		CallExprVisitor
		BasicLitVisitor
		ParenExprVisitor
		AssignVisitor
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
	case *CallExpr:
		n, ok := v.(CallExprVisitor)
		if ok {
			n.VisitCallExpr(t)
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
	case *Assign:
		n, ok := v.(AssignVisitor)
		if ok {
			n.VisitAssign(t)
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

	PartialStmtVisitor interface {
		VisitStmt(x Stmt)
	}
	ExprStmtVisitor interface {
		VisitExprStmt(x *ExprStmt)
	}
	PrintStmtVisitor interface {
		VisitPrintStmt(x *PrintStmt)
	}
	VarDeclVisitor interface {
		VisitVarDecl(x *VarDecl)
	}
	FunDeclVisitor interface {
		VisitFunDecl(x *FunDecl)
	}
	BlockStmtVisitor interface {
		VisitBlockStmt(x *BlockStmt)
	}
	IfStmtVisitor interface {
		VisitIfStmt(x *IfStmt)
	}
	WhileStmtVisitor interface {
		VisitWhileStmt(x *WhileStmt)
	}
	ForStmtVisitor interface {
		VisitForStmt(x *ForStmt)
	}
	ReturnStmtVisitor interface {
		VisitReturnStmt(x *ReturnStmt)
	}
	FullStmtVisitor interface {
		ExprStmtVisitor
		PrintStmtVisitor
		VarDeclVisitor
		FunDeclVisitor
		BlockStmtVisitor
		IfStmtVisitor
		WhileStmtVisitor
		ForStmtVisitor
		ReturnStmtVisitor
	}
)

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
	case *VarDecl:
		n, ok := v.(VarDeclVisitor)
		if ok {
			n.VisitVarDecl(t)
			return
		}
	case *FunDecl:
		n, ok := v.(FunDeclVisitor)
		if ok {
			n.VisitFunDecl(t)
			return
		}
	case *BlockStmt:
		n, ok := v.(BlockStmtVisitor)
		if ok {
			n.VisitBlockStmt(t)
			return
		}
	case *IfStmt:
		n, ok := v.(IfStmtVisitor)
		if ok {
			n.VisitIfStmt(t)
			return
		}
	case *WhileStmt:
		n, ok := v.(WhileStmtVisitor)
		if ok {
			n.VisitWhileStmt(t)
			return
		}
	case *ForStmt:
		n, ok := v.(ForStmtVisitor)
		if ok {
			n.VisitForStmt(t)
			return
		}
	case *ReturnStmt:
		n, ok := v.(ReturnStmtVisitor)
		if ok {
			n.VisitReturnStmt(t)
			return
		}
	}
	v.(PartialStmtVisitor).VisitStmt(e)
}
