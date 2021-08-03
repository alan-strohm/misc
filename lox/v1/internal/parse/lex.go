package parse

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

type Item struct {
	Type ItemType // The type of this item
	Pos  Pos      // The starting position, in bytes, of this item in the input string.
	Val  string   // The value of this item.
	Line int      // The line number at the start of this item.
}

func (i Item) isKeyword() bool {
	return i.Type > ItemKeyword
}

func (i Item) String() string {
	switch {
	case i.Type == ItemEOF:
		return "EOF"
	case i.Type == ItemError:
		return i.Val
	case i.isKeyword():
		return fmt.Sprintf("<%s>", i.Val)
	case len(i.Val) > 10:
		return fmt.Sprintf("%.10q...", i.Val)
	}
	return fmt.Sprintf("%q", i.Val)
}

const (
	lowestPrec  = 0
	unaryPrec   = 6
	highestPrec = 7
)

func (i ItemType) precedence() int {
	switch i {
	case ItemEqualEqual, ItemBangEqual, ItemGreater, ItemLess, ItemGreaterEqual, ItemLessEqual:
		return 3
	case ItemPlus, ItemMinus:
		return 4
	case ItemStar, ItemSlash:
		return 5
	}
	return lowestPrec
}

type ItemType int

const (
	ItemError ItemType = iota // error occurred; value is text of error
	ItemEOF
	ItemLeftParen
	ItemRightParen
	ItemLeftBrace
	ItemRightBrace
	ItemComma
	ItemDot
	ItemMinus
	ItemPlus
	ItemSemicolon
	ItemSlash
	ItemStar
	ItemBang
	ItemBangEqual
	ItemEqual
	ItemEqualEqual
	ItemGreater
	ItemGreaterEqual
	ItemLess
	ItemLessEqual
	ItemIdentifier
	ItemString // string literal (includes quotes)
	ItemNumber // number literal
	// Keywords appear after all the rest
	ItemKeyword // used only to delimit the keywords
	ItemAnd     // and keyword
	ItemClass   // class keyword
	ItemElse    // else keyword
	ItemFalse   // false literal
	ItemFun     // fun keyword
	ItemFor     // for keyword
	ItemIf      // if keyword
	ItemNil     // nil keyword
	ItemOr      // or keyword
	ItemPrint   // print keyword
	ItemReturn  // return keyword
	ItemSuper   // super keyword
	ItemThis    // this keyword
	ItemTrue    // true literal
	ItemVar     // var keyword
	ItemWhile   // while keyword
)

type lexer struct {
	input     string // the string being scanned
	pos       Pos    // current position in the input
	start     Pos    // start position of this item
	width     Pos    // width of the last rune read from input
	out       Item   // most recent item
	line      int    // 1+number of newlines seen
	startLine int    // start line of this item
}

const eof = -1

// next returns the next rune in the input
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = Pos(w)
	l.pos += l.width
	if r == '\n' {
		l.line++
	}
	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
	// Correct newline count
	if l.width == 1 && l.input[l.pos] == '\n' {
		l.line--
	}
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
	l.startLine = l.line
}

// emit passes an item back to the client
func (l *lexer) emit(t ItemType) {
	l.out = Item{t, l.start, l.input[l.start:l.pos], l.line}
	l.start = l.pos
	l.startLine = l.line
}

// emitError passes an error item back to the client.
func (l *lexer) emitError(format string, args ...interface{}) {
	l.out = Item{ItemError, l.start, fmt.Sprintf(format, args...), l.line}
	l.start = l.pos
	l.startLine = l.line
}

// accept consumes the next rune if it's from the valid set
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
}

// item  returns the next item from the input.
// Called by the parser.
func (l *lexer) item() Item {
	return l.out
}

var singleCharItems = map[rune]ItemType{
	'(': ItemLeftParen, ')': ItemRightParen,
	'{': ItemLeftBrace, '}': ItemRightBrace,
	',': ItemComma, '.': ItemDot,
	'-': ItemMinus, '+': ItemPlus,
	';': ItemSemicolon, '*': ItemStar,
	eof: ItemEOF,
}

func (l *lexer) scan() bool {
	if l.out.Type == ItemEOF {
		return false
	}
	l.acceptRun(" \r\t\n")
	l.ignore()
	switch r := l.next(); {
	case r == eof:
		l.emit(ItemEOF)
	case singleCharItems[r] != ItemError:
		l.emit(singleCharItems[r])
	case r == '!' && l.accept("="):
		l.emit(ItemBangEqual)
	case r == '!':
		l.emit(ItemBang)
	case r == '=' && l.accept("="):
		l.emit(ItemEqualEqual)
	case r == '=':
		l.emit(ItemEqual)
	case r == '<' && l.accept("="):
		l.emit(ItemLessEqual)
	case r == '<':
		l.emit(ItemLess)
	case r == '>' && l.accept("="):
		l.emit(ItemGreaterEqual)
	case r == '>':
		l.emit(ItemGreater)
	case r == '/':
		if l.accept("/") {
			for r := l.next(); r != '\n' && r != eof; r = l.next() {
			}
			l.backup()
			l.ignore()
			return l.scan()
		} else {
			l.emit(ItemSlash)
		}
	case r == '"':
		l.lexString()
	case isDigit(r):
		l.lexNumber()
	case isAlpha(r):
		l.lexIdentifier()
	default:
		l.emitError("unexpected character: %v", r)
	}
	return true
}

func (l *lexer) lexString() {
	r := l.next()
	for ; r != '"' && r != eof; r = l.next() {
		if r == '\n' {
			l.line++
		}
	}
	if r == eof {
		l.backup()
		l.emitError("unterminated string")
	} else {
		l.emit(ItemString)
	}
}

func isDigit(r rune) bool {
	return '0' <= r && r <= '9'
}

func (l *lexer) lexNumber() {
	digits := "0123456789"
	l.acceptRun(digits)
	if l.accept(".") && !isDigit(l.next()) {
		l.emitError("number with trailing .")
	} else {
		l.acceptRun(digits)
		l.emit(ItemNumber)
	}
}

func isAlpha(r rune) bool {
	return ('a' <= r && r <= 'z') ||
		('A' <= r && r <= 'Z') ||
		r == '_'
}

func isAlphanumeric(r rune) bool {
	return isAlpha(r) || isDigit(r)
}

var key = map[string]ItemType{
	"and":    ItemAnd,
	"class":  ItemClass,
	"else":   ItemElse,
	"false":  ItemFalse,
	"for":    ItemFor,
	"fun":    ItemFun,
	"if":     ItemIf,
	"nil":    ItemNil,
	"or":     ItemOr,
	"print":  ItemPrint,
	"return": ItemReturn,
	"super":  ItemSuper,
	"this":   ItemThis,
	"true":   ItemTrue,
	"var":    ItemVar,
	"while":  ItemWhile,
}

func (l *lexer) lexIdentifier() {
	for isAlphanumeric(l.next()) {
		// absorb
	}
	l.backup()
	word := l.input[l.start:l.pos]
	typ, ok := key[word]
	if !ok {
		typ = ItemIdentifier
	}
	l.emit(typ)
}

// lex creates a new scanner for the input string.
func lex(input string) *lexer {
	l := &lexer{
		input:     input,
		line:      1,
		startLine: 1,
	}
	return l
}
