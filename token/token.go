package token

type TokenType string
type Token struct {
	Type    TokenType
	Literal string
}

const (
	LET     TokenType = "LET"
	VOID    TokenType = "VOID"
	RETURN  TokenType = "RETURN"
	IF      TokenType = "IF"
	ELSE    TokenType = "ELSE"
	TRUE    TokenType = "TRUE"
	FALSE   TokenType = "FALSE"
	CLASS   TokenType = "CLASS"
	NEW     TokenType = "NEW"
	THIS    TokenType = "THIS"
	EXTENDS TokenType = "EXTENDS"
	MODULE  TokenType = "MODULE"
	LOG     TokenType = "LOG"
	STATIC  TokenType = "STATIC"

	PUBLIC    TokenType = "PUBLIC"
	PRIVATE   TokenType = "PRIVATE"
	PROTECTED TokenType = "PROTECTED"

	TYPE_INT     TokenType = "TYPE_INT"
	TYPE_STRING  TokenType = "TYPE_STRING"
	TYPE_BOOLEAN TokenType = "TYPE_BOOLEAN"
	TYPE_ANY     TokenType = "TYPE_ANY"

	// Operadores
	ASSIGN   TokenType = "="
	PLUS     TokenType = "+"
	MINUS    TokenType = "-"
	BANG     TokenType = "!"
	ASTERISK TokenType = "*"
	SLASH    TokenType = "/"
	LT       TokenType = "<"
	GT       TokenType = ">"
	EQ       TokenType = "=="
	NOT_EQ   TokenType = "!="
	ARROW    TokenType = "=>"

	COMMA     TokenType = ","
	SEMICOLON TokenType = ";"
	LPAREN    TokenType = "("
	RPAREN    TokenType = ")"
	LBRACE    TokenType = "{"
	RBRACE    TokenType = "}"
	LBRACKET  TokenType = "["
	RBRACKET  TokenType = "]"
	DOT       TokenType = "."
	COLON     TokenType = ":"

	IDENT  TokenType = "IDENT"
	INT    TokenType = "INT"
	STRING TokenType = "STRING"

	EOF     TokenType = "EOF"
	ILLEGAL TokenType = "ILLEGAL"
)

var keywords = map[string]TokenType{
	"let":       LET,
	"void":      VOID,
	"return":    RETURN,
	"if":        IF,
	"else":      ELSE,
	"true":      TRUE,
	"false":     FALSE,
	"class":     CLASS,
	"new":       NEW,
	"this":      THIS,
	"extends":   EXTENDS,
	"module":    MODULE,
	"log":       LOG,
	"static":    STATIC,
	"int":       TYPE_INT,
	"string":    TYPE_STRING,
	"boolean":   TYPE_BOOLEAN,
	"any":       TYPE_ANY,
	"public":    PUBLIC,
	"private":   PRIVATE,
	"protected": PROTECTED,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
