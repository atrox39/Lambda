// parser/parser.go
package parser

import (
	"fmt"
	"strconv"

	"github.com/atrox39/lambda/ast"
	"github.com/atrox39/lambda/lexer"
	"github.com/atrox39/lambda/token"
)

// Precedencia de operadores
const (
	_ int = iota
	LOWEST
	ASSIGN_PRECEDENCE // = (right-associative)
	EQUALS            // ==, !=
	LESSGREATER       // > o <
	SUM               // +
	PRODUCT           // *
	PREFIX            // -X o !X
	DOT_ACCESS        // object.member (higher than CALL)
	CALL              // myFunction(X)
	INDEX             // array[index]
)

// precedences mapea tipos de token a su nivel de precedencia.
var precedences = map[token.TokenType]int{
	token.ASSIGN:   ASSIGN_PRECEDENCE,
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL,
	token.LBRACKET: INDEX,
	token.DOT:      DOT_ACCESS,
}

// Parser es la estructura que construye el AST a partir de los tokens.
type Parser struct {
	l *lexer.Lexer // El lexer asociado

	currentToken token.Token // El token actual bajo examen
	peekToken    token.Token // El siguiente token (peek, no consumido)

	errors []string // Errores de parsing

	// Mapas para registrar funciones de parsing de prefijos y infijos.
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

// prefixParseFn es el tipo de función para parsear expresiones prefijo.
type prefixParseFn func() ast.Expression

// infixParseFn es el tipo de función para parsear expresiones infijo.
type infixParseFn func(ast.Expression) ast.Expression

// NewParser crea una nueva instancia del Parser.
func NewParser(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	// Registra las funciones de parsing de prefijos.
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.VOID, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_INT, p.parseFunctionLiteral) // Tipos también pueden iniciar literales de función (ej. int() { ... })
	p.registerPrefix(token.TYPE_STRING, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_BOOLEAN, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_ANY, p.parseFunctionLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)
	p.registerPrefix(token.NEW, p.parseNewExpression)
	p.registerPrefix(token.THIS, p.parseThisExpression)     // Nuevo

	// Registra las funciones de parsing de infijos.
	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)
	p.registerInfix(token.DOT, p.parseDotExpression)           // Nuevo: para acceso a miembros (p.saludar())
	p.registerInfix(token.ASSIGN, p.parseAssignmentExpression) // Nuevo: para asignaciones

	// Lee dos tokens para inicializar currentToken y peekToken.
	p.nextToken()
	p.nextToken()

	return p
}

// Errors devuelve los errores de parsing acumulados.
func (p *Parser) Errors() []string {
	return p.errors
}

// peekError añade un error si el siguiente token no es del tipo esperado.
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("Se esperaba el siguiente token de tipo %s, pero se obtuvo %s (%s)",
		t, p.peekToken.Type, p.peekToken.Literal)
	p.errors = append(p.errors, msg)
}

// nextToken avanza currentToken y peekToken.
func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// expectPeek verifica si el siguiente token es del tipo esperado y lo consume.
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

// currentTokenIs verifica si el token actual es del tipo dado.
func (p *Parser) currentTokenIs(t token.TokenType) bool {
	return p.currentToken.Type == t
}

// peekTokenIs verifica si el siguiente token es del tipo dado.
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// peekPrecedence devuelve la precedencia del siguiente token.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// currentPrecedence devuelve la precedencia del token actual.
func (p *Parser) currentPrecedence() int {
	if p, ok := precedences[p.currentToken.Type]; ok {
		return p
	}
	return LOWEST
}

// registerPrefix registra una función de parsing para un tipo de token prefijo.
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix registra una función de parsing para un tipo de token infijo.
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// ParseProgram parsea el programa completo y devuelve el AST.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.currentTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

// ---- CAMBIOS PRINCIPALES COMIENZAN AQUÍ ----
func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	case token.IF: // If puede ser una declaración
		expr := p.parseIfExpression()
		if expr == nil { return nil }
		// El AST de IfExpression ya es un Statement.
		if stmt, ok := expr.(ast.Statement); ok {
			// Si se requiere punto y coma opcional después de if-statement
			if p.peekTokenIs(token.SEMICOLON) {p.nextToken()}
			return stmt
		}
		p.errors = append(p.errors, "Error: 'if' expression no pudo ser usado como statement.")
		return nil
	case token.CLASS: // Nueva declaración de clase
		return p.parseClassStatement(nil) // Nil indica que no hay modificador previo
	case token.PUBLIC, token.PRIVATE, token.PROTECTED: // Modificador al inicio
		return p.parseStatementWithModifier()
	case token.LOG:
		return p.parseLogStatement()
	// VOID o TYPE al inicio de un statement puede ser una declaración de función global
	// o un literal de función usado como expresión en un ExpressionStatement.
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY:
		// Para distinguir, necesitamos ver si sigue un IDENT (nombre de func) y luego LPAREN.
		// Esto es complicado sin lookahead > 1.
		// Heurística: si después de (TYPE|VOID) hay IDENT y luego LPAREN, es FunctionDeclaration.
		// Esta heurística es imperfecta. Monkey lo resuelve porque FunctionLiteral no es un Statement.
		if p.peekTokenIs(token.IDENT) && p.isFunctionDeclarationAhead() {
			return p.parseFunctionDeclaration(p.currentToken) // Pasar el tipo/void como token inicial
		}
		// Si no, es un ExpressionStatement (ej. (void(){})() o un tipo solo que es error)
		return p.parseExpressionStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// isFunctionDeclarationAhead es una heurística.
// Verifica si después del actual IDENT (que es p.peekToken), sigue un LPAREN.
// Se llama cuando currentToken es TYPE/VOID y peekToken es IDENT.
func (p *Parser) isFunctionDeclarationAhead() bool {
    // Necesitamos mirar p.peekToken (que es IDENT) y el token DESPUÉS de ese.
    // Esto es LL(2). El lexer actual no lo soporta directamente.
    // Esta función es una simplificación conceptual.
    // Una implementación real requeriría un lexer con buffer o parseo especulativo.
    // Por ahora, esta función no puede implementarse de forma robusta.
    // La lógica en parseStatement se basará en una detección más simple o aceptará ambigüedad
    // que se resolvería semánticamente o con errores de parsing más adelante.
	// Para una prueba simple, si el siguiente al IDENT (p.peekToken) es LPAREN, es probable.
	// Esto es lo que el parser de Pratt hace implícitamente para llamadas.
	// No podemos hacer esto aquí sin cambiar el estado del parser o lexer.
	// Devolvemos false para evitar complejidad ahora.
	// El parser de Monkey original no permite `func foo() {}` como statement directamente,
	// sino `let foo = func() {};`
	// Si Lambda permite `void foo() {}` a nivel global, esta detección es crucial.
    return false // Simplificación: esta heurística es difícil de implementar correctamente aquí.
                 // El parser de Monkey original no tiene declaraciones de función nombradas globales así.
                 // Las funciones son expresiones asignadas a 'let'.
                 // Si Lambda sí las tiene, esto necesita una solución LL(2).
}

// parseStatementWithModifier maneja declaraciones que comienzan con public, private, protected.
func (p *Parser) parseStatementWithModifier() ast.Statement {
	modifier := p.currentToken
	p.nextToken() // Consume el modificador

	switch p.currentToken.Type {
	case token.CLASS:
		return p.parseClassStatement(&modifier) // Pasar el modificador
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY:
		// Es una declaración de función con modificador: public void miFuncion()...
		// p.currentToken es ahora el tipo/void.
		return p.parseFunctionDeclaration(modifier) // El parser de función manejará el tipo actual
	default:
		p.errors = append(p.errors, fmt.Sprintf("Token inesperado '%s' después del modificador '%s'", p.currentToken.Literal, modifier.Literal))
		return nil
	}
}
// parseLetStatement parsea una declaración 'let'.
func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if p.peekTokenIs(token.COLON) {
		p.nextToken()
		if !p.expectPeekIsTypeOrIdent() {
			return nil
		}
		stmt.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.ASSIGN) {
		p.peekError(token.ASSIGN)
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) expectPeekIsTypeOrIdent() bool {
	if !p.peekTokenIsTypeOrVoid() && !p.peekTokenIs(token.IDENT) {
		p.peekError(token.IDENT)
		return false
	}
	p.nextToken()
	return true
}

func (p *Parser) peekTokenIsTypeOrVoid() bool {
	tt := p.peekToken.Type
	return tt == token.TYPE_INT || tt == token.TYPE_STRING || tt == token.TYPE_BOOLEAN || tt == token.TYPE_ANY || tt == token.VOID
}

func (p *Parser) currentTokenIsTypeOrVoid() bool {
	tt := p.currentToken.Type
	return tt == token.TYPE_INT || tt == token.TYPE_STRING || tt == token.TYPE_BOOLEAN || tt == token.TYPE_ANY || tt == token.VOID
}


// peekTokenIsType verifica si el siguiente token es un tipo de dato conocido o IDENT (para clases).
func (p *Parser) peekTokenIsType() bool {
	switch p.peekToken.Type {
	case token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.VOID, token.TYPE_ANY, token.IDENT:
		return true
	default:
		return false
	}
}

// currentTokenIsType verifica si el token actual es un tipo de dato conocido o IDENT.
func (p *Parser) currentTokenIsType() bool {
	switch p.currentToken.Type {
	case token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.VOID, token.TYPE_ANY, token.IDENT:
		return true
	default:
		return false
	}
}
func (p *Parser) currentTokenIsModifier() bool {
	switch p.currentToken.Type {
	case token.PUBLIC, token.PRIVATE, token.PROTECTED:
		return true
	default:
		return false
	}
}

// parseReturnStatement parsea una declaración 'return'.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.currentToken}

	p.nextToken() // Avanza al inicio de la expresión de retorno

	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseExpressionStatement parsea una expresión usada como declaración.
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}
	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseExpressionStatement parsea una expresión usada como declaración.
func (p *Parser) parseExpressionStatement() ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}
	stmt.Expression = p.parseExpression(LOWEST)
	// Semicolon opcional para expression statements, como en JS.
	// Si es la última línea de un bloque o programa, no es necesario.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

// parseExpressionStatement parsea una expresión usada como declaración.
func (p *Parser) parseExpressionStatement() ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}
	stmt.Expression = p.parseExpression(LOWEST)
	// Semicolon opcional para expression statements, como en JS.
	// Si es la última línea de un bloque o programa, no es necesario.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

// parseIdentifier parsea un identificador.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}

// parseIntegerLiteral parsea un literal entero.
func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.currentToken}

	value, err := strconv.ParseInt(p.currentToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("No se pudo parsear %q como entero", p.currentToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

// parseStringLiteral parsea un literal de cadena.
func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.currentToken, Value: p.currentToken.Literal}
}

// parsePrefixExpression parsea una expresión prefijo.
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

// parseInfixExpression parsea una expresión infijo.
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}
	precedence := p.currentPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

// parseAssignmentExpression parsea una expresión de asignación.
// 'left' es la expresión en el lado izquierdo de la asignación (ej. identificador, expresión de punto).
// El token ASSIGN (p.currentToken) ya ha sido consumido por el bucle de parseExpression.
func (p *Parser) parseAssignmentExpression(left ast.Expression) ast.Expression {
	exp := &ast.AssignmentExpression{
		Token: p.currentToken, // Este es el token ASSIGN
		Left:  left,
	}
	// Avanza al siguiente token después del operador ASSIGN (el inicio de la expresión de valor)
	p.nextToken()
	// Parsea el lado derecho con la precedencia más baja para asegurar que se parsee completamente.
	exp.Value = p.parseExpression(LOWEST)
	return exp
}

// parseBoolean parsea un literal booleano.
func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.currentToken, Value: p.currentTokenIs(token.TRUE)}
}

// parseGroupedExpression parsea una expresión entre paréntesis.
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken() // Consume el LPAREN
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

// parseFunctionLiteral parsea una expresión de función (ej. void(int x, string y) { ... }).
func (p *Parser) parseFunctionLiteral() ast.Expression {
	// currentToken es el tipo de retorno (ej. VOID, TYPE_INT, etc.)
	lit := &ast.FunctionLiteral{Token: p.currentToken, ReturnType: &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

// parseFunctionDeclaration parsea una declaración de función (void funcName(int a, string b): int { ... }).
func (p *Parser) parseFunctionDeclaration() *ast.FunctionDeclaration {
	// currentToken es el tipo de retorno (ej. VOID, TYPE_INT, etc.)
	decl := &ast.FunctionDeclaration{Token: p.currentToken, ReturnType: &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}}

	if !p.expectPeek(token.IDENT) { // Espera el nombre de la función
		return nil
	}
	decl.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.LPAREN) { // Espera '(' para los parámetros
		return nil
	}

	decl.Parameters = p.parseFunctionParameters()

	if p.peekTokenIs(token.COLON) { // Opcional: tipo de retorno explícito después de los parámetros
		p.nextToken() // Consume COLON
		if !p.peekTokenIsType() {
			p.peekError(token.IDENT)
			return nil
		}
		p.nextToken() // Consume el token de tipo
		decl.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) { // Espera '{' para el cuerpo
		return nil
	}

	decl.Body = p.parseBlockStatement()

	return decl
}

// parseFunctionParameters parsea los parámetros de una función, incluyendo sus tipos.
// Espera una lista como (nombre: string, edad: int)
func (p *Parser) parseFunctionParameters() []*ast.Parameter {
	parameters := []*ast.Parameter{}

	if p.peekTokenIs(token.RPAREN) { // Caso de función sin parámetros: `()`
		p.nextToken()
		return parameters
	}

	p.nextToken() // Consume el primer token (que debería ser un IDENT)

	// Primer parámetro
	param := &ast.Parameter{}
	if p.currentTokenIs(token.IDENT) { // Espera el nombre del parámetro
		param.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

		if p.peekTokenIs(token.COLON) { // Opcional: anotación de tipo
			p.nextToken()             // Consume COLON
			if !p.peekTokenIsType() { // Espera un token de tipo
				p.peekError(token.IDENT) // Podría ser un IDENT que actúa como tipo (ej. nombre de clase)
				return nil
			}
			p.nextToken() // Consume el token de tipo
			param.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
		}
	} else {
		p.errors = append(p.errors, fmt.Sprintf("Se esperaba un identificador para el parámetro, pero se obtuvo %s (%s)", p.currentToken.Type, p.currentToken.Literal))
		return nil
	}
	parameters = append(parameters, param)

	// Parámetros subsiguientes
	for p.peekTokenIs(token.COMMA) {
		p.nextToken() // Consume la coma
		p.nextToken() // Consume el siguiente token (que debería ser un IDENT)

		param = &ast.Parameter{}
		if p.currentTokenIs(token.IDENT) { // Espera el nombre del parámetro
			param.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

			if p.peekTokenIs(token.COLON) { // Opcional: anotación de tipo
				p.nextToken()             // Consume COLON
				if !p.peekTokenIsType() { // Espera un token de tipo
					p.peekError(token.IDENT)
					return nil
				}
				p.nextToken() // Consume el token de tipo
				param.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
			}
		} else {
			p.errors = append(p.errors, fmt.Sprintf("Se esperaba un identificador para el parámetro, pero se obtuvo %s (%s)", p.currentToken.Type, p.currentToken.Literal))
			return nil
		}
		parameters = append(parameters, param)
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return parameters
}

// parseCallExpression parsea una llamada a función.
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.currentToken, Function: function}
	exp.Arguments = p.parseCallArguments()
	return exp
}

// parseCallArguments parsea los argumentos de una llamada a función.
func (p *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken() // Consume el primer argumento

	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken() // Consume la coma
		p.nextToken() // Consume el siguiente argumento
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return args
}

// parseBlockStatement parsea un bloque de código (ej. el cuerpo de una función o un if/else).
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken() // Consume el LBRACE

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		// Solo avanzamos el token si el statement no consumió el RBRACE o EOF
		if !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
			p.nextToken()
		}
	}
	return block
}

// parseIfStatement parsea una declaración 'if'.
func (p *Parser) parseIfStatement() ast.Statement {
	expression := &ast.IfExpression{Token: p.currentToken}

	if !p.expectPeek(token.LPAREN) { // Espera '(' después de 'if'
		return nil
	}

	p.nextToken() // Avanza a la condición
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) { // Espera ')' después de la condición
		return nil
	}

	if !p.expectPeek(token.LBRACE) { // Espera '{' para el bloque de consecuencia
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) { // Si hay un 'else'
		p.nextToken()                    // Consume 'else'
		if !p.expectPeek(token.LBRACE) { // Espera '{' para el bloque alternativo
			return nil
		}
		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseLogStatement() ast.Statement {
	stmt := &ast.LogStatement{Token: p.currentToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()
	stmt.Argument = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseArrayLiteral parsea un literal de array.
func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.currentToken}
	array.Elements = p.parseExpressionList(token.RBRACKET)
	return array
}

// parseExpressionList es una función auxiliar para parsear listas de expresiones (ej. argumentos de función, elementos de array).
func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken() // Consume el primer elemento

	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken() // Consume la coma
		p.nextToken() // Consume el siguiente elemento
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

// parseIndexExpression parsea una expresión de índice.
func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.currentToken, Left: left}

	p.nextToken() // Consume el LBRACKET
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

// parseModuleStatement parsea una declaración 'module'.
func (p *Parser) parseModuleStatement() *ast.ModuleStatement {
	stmt := &ast.ModuleStatement{Token: p.currentToken}

	p.nextToken() // Consume 'module'

	switch p.currentToken.Type {
	case token.LET:
		letStmt := p.parseLetStatement()
		if letStmt == nil {
			return nil
		}
		stmt.Value = letStmt
		stmt.Name = letStmt.Name
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY: // Es una declaración de función con tipo de retorno
		// currentToken es el tipo de retorno.
		// Peek el IDENT para el nombre de la función para diferenciar de FunctionLiteral.
		if p.peekTokenIs(token.IDENT) {
			funcDecl := p.parseFunctionDeclaration()
			if funcDecl == nil {
				return nil
			}
			stmt.Value = funcDecl
			stmt.Name = funcDecl.Name
		} else {
			p.errors = append(p.errors, fmt.Sprintf("Se esperaba un identificador de función después del tipo de retorno en declaración 'module', pero se obtuvo %s (%s)", p.peekToken.Type, p.peekToken.Literal))
			return nil
		}
	case token.CLASS:
		classStmt := p.parseClassStatement()
		if classStmt == nil {
			return nil
		}
		stmt.Value = classStmt
		stmt.Name = classStmt.Name
	default:
		p.errors = append(p.errors, fmt.Sprintf("Tipo de declaración no soportado para 'module': %s", p.currentToken.Type))
		return nil
	}

	// Para declaraciones simples como 'module let x = 10;', se consume un punto y coma.
	// Las funciones y clases tienen sus propios bloques.
	_, isLet := stmt.Value.(*ast.LetStatement)
	if isLet && p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseClassStatement parsea una declaración 'class'.
func (p *Parser) parseClassStatement() *ast.ClassStatement {
	stmt := &ast.ClassStatement{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) { // Espera el nombre de la clase
		return nil
	}
	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if p.peekTokenIs(token.EXTENDS) { // Opcional: herencia
		p.nextToken()                   // Consume EXTENDS
		if !p.expectPeek(token.IDENT) { // Espera el nombre de la superclase
			return nil
		}
		stmt.SuperClass = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) { // Espera '{' para el cuerpo de la clase
		return nil
	}

	stmt.Body = p.parseClassBody(stmt.Name.Value) // Pasa el nombre de la clase para identificar el constructor

	return stmt
}

// parseClassBody parsea el cuerpo de una clase, incluyendo métodos y propiedades.
func (p *Parser) parseClassBody(className string) *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken() // Consume el LBRACE

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		var member ast.Statement = nil
		// Prioridad: Constructor > Método > Propiedad

		// 1. Intentar parsear como Constructor: ClassName(args) { ... }
		if p.currentTokenIs(token.IDENT) && p.currentToken.Literal == className && p.peekTokenIs(token.LPAREN) {
			member = p.parseMethodDeclaration(true) // true indica que es constructor
		} else if p.currentTokenIs(token.VOID) || p.currentTokenIsType() {
			// 2. Intentar parsear como Método: Type methodName(args): Type { ... }
			// Asegurarse de que no sea una expresión de función anónima (void(args){...})
			// La diferencia es que un método siempre tiene un nombre después del tipo de retorno.
			if p.peekTokenIs(token.IDENT) { // Si después del tipo, hay un IDENT, es un método.
				member = p.parseMethodDeclaration(false) // false indica que es un método regular
			} else {
				// Esto podría ser una expresión de función anónima (void(x){...})
				// o un tipo de retorno sin un nombre de método, lo cual es un error en el cuerpo de la clase.
				p.errors = append(p.errors, fmt.Sprintf("Declaración de método o expresión inesperada en el cuerpo de la clase: %s (%s)", p.currentToken.Type, p.currentToken.Literal))
				p.nextToken() // Avanza para evitar bucle infinito
				continue
			}
		} else if p.currentTokenIs(token.LET) {
			// 3. Intentar parsear como Propiedad: let propertyName: Type = value;
			member = p.parsePropertyDeclaration()
		} else {
			// Error o algo no reconocido en el cuerpo de la clase
			p.errors = append(p.errors, fmt.Sprintf("Token inesperado en el cuerpo de la clase: %s (%s)", p.currentToken.Type, p.currentToken.Literal))
			p.nextToken() // Avanza para evitar bucle infinito
			continue
		}

		if member != nil {
			block.Statements = append(block.Statements, member)
		}
		// Solo avanzamos el token si el statement no consumió el RBRACE o EOF
		if !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
			p.nextToken()
		}
	}
	return block
}

// parseMethodDeclaration parsea un método o constructor dentro de una clase.
func (p *Parser) parseMethodDeclaration(isConstructor bool) *ast.MethodDeclaration {
	method := &ast.MethodDeclaration{Token: p.currentToken, IsConstructor: isConstructor}

	if !isConstructor {
		// currentToken es el tipo de retorno (VOID, TYPE_INT, etc.)
		if !p.currentTokenIsType() {
			p.errors = append(p.errors, fmt.Sprintf("Se esperaba un tipo de retorno para el método, pero se obtuvo %s (%s)", p.currentToken.Type, p.currentToken.Literal))
			return nil
		}
		method.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

		if !p.expectPeek(token.IDENT) { // Espera el nombre del método
			return nil
		}
	} else {
		// currentToken es el IDENT del constructor (nombre de la clase).
		// No tiene tipo de retorno explícito en la sintaxis.
		method.ReturnType = nil
	}

	method.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.LPAREN) { // Espera '(' para los parámetros
		return nil
	}

	method.Parameters = p.parseFunctionParameters() // Reutiliza la lógica de parámetros

	if p.peekTokenIs(token.COLON) && !isConstructor { // Opcional: tipo de retorno explícito después de los paréntesis para métodos
		p.nextToken() // Consume COLON
		if !p.peekTokenIsType() {
			p.peekError(token.IDENT)
			return nil
		}
		p.nextToken() // Consume el token de tipo
		method.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) { // Espera '{' para el cuerpo
		return nil
	}

	method.Body = p.parseBlockStatement() // Reutiliza la lógica de bloque

	return method
}

// PropertyDeclaration representa una propiedad dentro de una clase.
func (p *Parser) parsePropertyDeclaration() *ast.PropertyDeclaration {
	prop := &ast.PropertyDeclaration{Token: p.currentToken} // currentToken es LET

	if !p.expectPeek(token.IDENT) { // Espera el nombre de la propiedad
		return nil
	}
	prop.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if p.peekTokenIs(token.COLON) { // Opcional: anotación de tipo
		p.nextToken()             // Consume COLON
		if !p.peekTokenIsType() { // Espera un token de tipo
			p.peekError(token.IDENT) // Podría ser un IDENT que actúa como tipo (ej. nombre de clase)
			return nil
		}
		p.nextToken() // Consume el token de tipo
		prop.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if p.peekTokenIs(token.ASSIGN) { // Propiedad con valor inicial
		p.nextToken() // Consume ASSIGN
		p.nextToken() // Avanza al inicio de la expresión de valor
		prop.Value = p.parseExpression(LOWEST)
	}

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	} else {
		p.errors = append(p.errors, fmt.Sprintf("Se esperaba ';' después de la declaración de propiedad, pero se obtuvo %s (%s)", p.peekToken.Type, p.peekToken.Literal))
	}

	return prop
}

// parseExpression parsea una expresión con el algoritmo de precedencia de operadores.
func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.currentToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.currentToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken() // Consume el operador infijo
		leftExp = infix(leftExp)
	}
	return leftExp
}

// noPrefixParseFnError añade un error si no hay una función de parsing prefijo para el token actual.
func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("No se encontró una función de parsing prefijo para %s", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseDotExpression(left ast.Expression) ast.Expression {
	exp := &ast.DotExpression{Token: p.currentToken, Left: left}
	if !p.expectPeek(token.IDENT) {
		return nil
	}
	exp.Member = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	return exp
}

func (p *Parser) parseNewExpression() ast.Expression {
	exp := &ast.NewExpression{Token: p.currentToken}
	if !p.expectPeek(token.IDENT) { // Espera el nombre de la clase
		return nil
	}
	exp.Class = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	if !p.expectPeek(token.LPAREN) { // Espera '(' para los argumentos del constructor
		return nil
	}
	exp.Arguments = p.parseCallArguments() // Reutiliza parseCallArguments
	return exp
}