package parse

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/alan-strohm/misc/lox/v1/internal/token"
)

type lexer struct {
	input     string      // the string being scanned
	pos       token.Pos   // current position in the input
	start     token.Pos   // start position of this token
	width     token.Pos   // width of the last rune read from input
	out       token.Token // most recent token
	line      int         // 1+number of newlines seen
	startLine int         // start line of this token
}

const eof = -1

// next returns the next rune in the input
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = token.Pos(w)
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

// emit passes an token back to the client
func (l *lexer) emit(t token.Type) {
	l.out = token.Token{t, l.start, l.input[l.start:l.pos]}
	l.start = l.pos
	l.startLine = l.line
}

// emitError passes an illegal token back to the client.
func (l *lexer) emitError(format string, args ...interface{}) {
	l.out = token.Token{token.ILLEGAL, l.start, fmt.Sprintf(format, args...)}
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

// token returns the next token from the input.
// Called by the parser.
func (l *lexer) token() token.Token {
	return l.out
}

var singleCharTokens = map[rune]token.Type{
	'(': token.LPAREN, ')': token.RPAREN,
	'{': token.LBRACE, '}': token.RBRACE,
	',': token.COMMA, '.': token.PERIOD,
	'-': token.SUB, '+': token.ADD,
	';': token.SEMICOLON, '*': token.MUL,
	eof: token.EOF,
}

func (l *lexer) scan() bool {
	if l.out.Type == token.EOF {
		return false
	}
	l.acceptRun(" \r\t\n")
	l.ignore()
	switch r := l.next(); {
	case r == eof:
		l.emit(token.EOF)
	case singleCharTokens[r] != token.ILLEGAL:
		l.emit(singleCharTokens[r])
	case r == '!' && l.accept("="):
		l.emit(token.NEQ)
	case r == '!':
		l.emit(token.NOT)
	case r == '=' && l.accept("="):
		l.emit(token.EQL)
	case r == '=':
		l.emit(token.ASSIGN)
	case r == '<' && l.accept("="):
		l.emit(token.LEQ)
	case r == '<':
		l.emit(token.LSS)
	case r == '>' && l.accept("="):
		l.emit(token.GEQ)
	case r == '>':
		l.emit(token.GTR)
	case r == '/':
		if l.accept("/") {
			for r := l.next(); r != '\n' && r != eof; r = l.next() {
			}
			l.backup()
			l.ignore()
			return l.scan()
		} else {
			l.emit(token.QUO)
		}
	case r == '"':
		l.lexString()
	case isDigit(r):
		l.lexNumber()
	case isAlpha(r):
		l.lexIdentifier()
	default:
		l.emitError("unexpected character: %#U", r)
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
		l.emit(token.STRING)
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
		l.emit(token.NUMBER)
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

func (l *lexer) lexIdentifier() {
	for isAlphanumeric(l.next()) {
		// absorb
	}
	l.backup()
	word := l.input[l.start:l.pos]
	l.emit(token.Lookup(word))
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
