// token/token.go
package token

// TokenType representa el tipo de un token.
type TokenType string

// Token representa una unidad léxica del código fuente.
type Token struct {
	Type    TokenType
	Literal string
}

// Definición de los tipos de tokens para el lenguaje Lambda.
const (
	// Palabras clave
	LET     TokenType = "LET"     // para 'let'
	VOID    TokenType = "VOID"    // para 'void' (reemplaza 'function' como tipo de retorno o palabra clave de función)
	RETURN  TokenType = "RETURN"  // para 'return'
	IF      TokenType = "IF"      // para 'if'
	ELSE    TokenType = "ELSE"    // para 'else'
	TRUE    TokenType = "TRUE"    // para 'true'
	FALSE   TokenType = "FALSE"   // para 'false'
	CLASS   TokenType = "CLASS"   // para 'class'
	NEW     TokenType = "NEW"     // para 'new'
	THIS    TokenType = "THIS"    // para 'this'
	EXTENDS TokenType = "EXTENDS" // para 'extends'
	MODULE  TokenType = "MODULE"  // para 'module' (nueva palabra clave)
	LOG     TokenType = "LOG"     // para 'log'

	// Tipos de datos (nuevos tokens)
	TYPE_INT     TokenType = "TYPE_INT"     // para 'int'
	TYPE_STRING  TokenType = "TYPE_STRING"  // para 'string'
	TYPE_BOOLEAN TokenType = "TYPE_BOOLEAN" // para 'boolean'
	TYPE_ANY     TokenType = "TYPE_ANY"     // para 'any'

	// Operadores
	ASSIGN   TokenType = "="  // Asignación
	PLUS     TokenType = "+"  // Suma
	MINUS    TokenType = "-"  // Resta
	BANG     TokenType = "!"  // Negación lógica
	ASTERISK TokenType = "*"  // Multiplicación
	SLASH    TokenType = "/"  // División
	LT       TokenType = "<"  // Menor que
	GT       TokenType = ">"  // Mayor que
	EQ       TokenType = "==" // Igualdad
	NOT_EQ   TokenType = "!=" // Desigualdad
	ARROW    TokenType = "=>" // Función flecha

	// Delimitadores
	COMMA     TokenType = "," // Coma
	SEMICOLON TokenType = ";" // Punto y coma
	LPAREN    TokenType = "(" // Paréntesis izquierdo
	RPAREN    TokenType = ")" // Paréntesis derecho
	LBRACE    TokenType = "{" // Llave izquierda
	RBRACE    TokenType = "}" // Llave derecha
	LBRACKET  TokenType = "[" // Corchete izquierdo
	RBRACKET  TokenType = "]" // Corchete derecho
	DOT       TokenType = "." // Punto para acceso a miembros
	COLON     TokenType = ":" // Dos puntos para anotaciones de tipo (nuevo token)

	// Tipos literales
	IDENT  TokenType = "IDENT"  // Identificadores (nombres de variables, funciones, clases)
	INT    TokenType = "INT"    // Enteros
	STRING TokenType = "STRING" // Cadenas de texto

	// Otros
	EOF     TokenType = "EOF"     // Fin del archivo
	ILLEGAL TokenType = "ILLEGAL" // Carácter ilegal
)

// keywords mapea las palabras clave de Lambda a sus tipos de token.
var keywords = map[string]TokenType{
	"let":     LET,
	"void":    VOID, // 'void' como palabra clave para funciones y tipo de retorno
	"return":  RETURN,
	"if":      IF,
	"else":    ELSE,
	"true":    TRUE,
	"false":   FALSE,
	"class":   CLASS,
	"new":     NEW,
	"this":    THIS,
	"extends": EXTENDS,
	"module":  MODULE,
	"log":     LOG,
	"int":     TYPE_INT,     // Nuevo tipo
	"string":  TYPE_STRING,  // Nuevo tipo
	"boolean": TYPE_BOOLEAN, // Nuevo tipo
	"any":     TYPE_ANY,     // Nuevo tipo
}

// LookupIdent determina si un identificador es una palabra clave o un IDENT.
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
