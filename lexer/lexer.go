// lexer/lexer.go
package lexer

import (
	"unicode"

	"github.com/atrox39/lambda/token"
)

// Lexer es la estructura que se encarga de tokenizar el código fuente.
type Lexer struct {
	input        string // El código fuente de Lambda
	position     int    // Posición actual en el input (apunta al carácter actual)
	readPosition int    // Siguiente posición de lectura (después del carácter actual)
	ch           byte   // Carácter actual bajo examen
}

// NewLexer crea una nueva instancia del Lexer.
func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar() // Inicializa el lexer leyendo el primer carácter
	return l
}

// readChar lee el siguiente carácter y avanza la posición.
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // 0 significa EOF (End Of File)
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
}

// peekChar devuelve el siguiente carácter sin avanzar la posición.
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// NextToken devuelve el siguiente token del input.
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace() // Ignora espacios en blanco

	switch l.ch {
	case '=':
		if l.peekChar() == '=' { // Para '=='
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else if l.peekChar() == '>' { // Para '=>' (función flecha)
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.ARROW, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '!':
		if l.peekChar() == '=' { // Para '!='
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case '.':
		tok = newToken(token.DOT, l.ch)
	case ':':
		tok = newToken(token.COLON, l.ch)
	case 0: // EOF
		tok.Literal = ""
		tok.Type = token.EOF
	case '"': // Cadenas de texto
		tok.Type = token.STRING
		tok.Literal = l.readString()
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal) // Verifica si es palabra clave (incluyendo nuevos tipos)
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar() // Avanza al siguiente carácter después de procesar el token
	return tok
}

// newToken es una función auxiliar para crear un nuevo token.
func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

// readIdentifier lee un identificador (letras y guiones bajos).
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readNumber lee un número (dígitos).
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readString lee una cadena de texto entre comillas dobles.
func (l *Lexer) readString() string {
	position := l.position + 1 // Ignora la comilla inicial
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 { // Termina en comilla o EOF
			break
		}
	}
	return l.input[position:l.position]
}

// skipWhitespace avanza el lexer sobre los espacios en blanco y comentarios de una línea.
func (l *Lexer) skipWhitespace() {
	for {
		if unicode.IsSpace(rune(l.ch)) {
			l.readChar()
		} else if l.ch == '/' && l.peekChar() == '/' { // Maneja comentarios de una sola línea
			for l.ch != '\n' && l.ch != 0 { // Lee hasta el salto de línea o EOF
				l.readChar()
			}
			l.readChar() // Consume el salto de línea
		} else {
			break
		}
	}
}

// isLetter verifica si un carácter es una letra o guion bajo.
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// isDigit verifica si un carácter es un dígito.
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}