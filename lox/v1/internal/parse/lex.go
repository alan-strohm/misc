package parse

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

type item struct {
	typ  itemType // The type of this item
	val  string   // The value of this item.
	line int      // The line number at the start of this item.
}

func (i item) isKeyword() bool {
	return i.typ > itemKeyword
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.isKeyword():
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

type itemType int

const (
	itemError itemType = iota // error occurred; value is text of error
	itemEOF
	itemLeftParen
	itemRightParen
	itemLeftBrace
	itemRightBrace
	itemComma
	itemDot
	itemMinus
	itemPlus
	itemSemicolon
	itemSlash
	itemStar
	itemBang
	itemBangEqual
	itemEqual
	itemEqualEqual
	itemGreater
	itemGreaterEqual
	itemLess
	itemLessEqual
	itemIdentifier
	itemString // string literal (includes quotes)
	itemNumber // number literal
	// Keywords appear after all the rest
	itemKeyword // used only to delimit the keywords
	itemAnd     // and keyword
	itemClass   // class keyword
	itemElse    // else keyword
	itemFalse   // false literal
	itemFun     // fun keyword
	itemFor     // for keyword
	itemIf      // if keyword
	itemNil     // nil keyword
	itemOr      // or keyword
	itemPrint   // print keyword
	itemReturn  // return keyword
	itemSuper   // super keyword
	itemThis    // this keyword
	itemTrue    // true literal
	itemVar     // var keyword
	itemWhile   // while keyword
)

type Pos int

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

type lexer struct {
	input     string // the string being scanned
	pos       Pos    // current position in the input
	start     Pos    // start position of this item
	width     Pos    // width of the last rune read from input
	out       item   // most recent item
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
func (l *lexer) emit(t itemType) {
	l.out = item{t, l.input[l.start:l.pos], l.line}
	l.start = l.pos
	l.startLine = l.line
}

// emitError passes an error item back to the client.
func (l *lexer) emitError(format string, args ...interface{}) {
	l.out = item{itemError, fmt.Sprintf(format, args...), l.line}
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
func (l *lexer) item() item {
	return l.out
}

var singleCharItems = map[rune]itemType{
	'(': itemLeftParen, ')': itemRightParen,
	'{': itemLeftBrace, '}': itemRightBrace,
	',': itemComma, '.': itemDot,
	'-': itemMinus, '+': itemPlus,
	';': itemSemicolon, '*': itemStar,
	eof: itemEOF,
}

func (l *lexer) scan() bool {
	if l.out.typ == itemEOF {
		return false
	}
	l.acceptRun(" \r\t\n")
	l.ignore()
	switch r := l.next(); {
	case r == eof:
		l.emit(itemEOF)
	case singleCharItems[r] != itemError:
		l.emit(singleCharItems[r])
	case r == '!' && l.accept("="):
		l.emit(itemBangEqual)
	case r == '!':
		l.emit(itemBang)
	case r == '=' && l.accept("="):
		l.emit(itemEqualEqual)
	case r == '=':
		l.emit(itemEqual)
	case r == '<' && l.accept("="):
		l.emit(itemLessEqual)
	case r == '<':
		l.emit(itemLess)
	case r == '>' && l.accept("="):
		l.emit(itemGreaterEqual)
	case r == '>':
		l.emit(itemGreater)
	case r == '/':
		if l.accept("/") {
			for r := l.next(); r != '\n' && r != eof; r = l.next() {
			}
			l.backup()
			l.ignore()
			return l.scan()
		} else {
			l.emit(itemSlash)
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
		l.emit(itemString)
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
		l.emit(itemNumber)
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

var key = map[string]itemType{
	"and":    itemAnd,
	"class":  itemClass,
	"else":   itemElse,
	"false":  itemFalse,
	"for":    itemFor,
	"fun":    itemFun,
	"if":     itemIf,
	"nil":    itemNil,
	"or":     itemOr,
	"print":  itemPrint,
	"return": itemReturn,
	"super":  itemSuper,
	"this":   itemThis,
	"true":   itemTrue,
	"var":    itemVar,
	"while":  itemWhile,
}

func (l *lexer) lexIdentifier() {
	for isAlphanumeric(l.next()) {
		// absorb
	}
	l.backup()
	word := l.input[l.start:l.pos]
	typ, ok := key[word]
	if !ok {
		typ = itemIdentifier
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
