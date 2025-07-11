package parser

import (
	"fmt"
	"strconv"

	"github.com/atrox39/lambda/ast"
	"github.com/atrox39/lambda/lexer"
	"github.com/atrox39/lambda/token"
)

const (
	_ int = iota
	LOWEST
	ASSIGN_PRECEDENCE
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	DOT_ACCESS
	CALL
	INDEX
)

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

type Parser struct {
	l *lexer.Lexer

	currentToken token.Token
	peekToken    token.Token

	errors []string

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

type prefixParseFn func() ast.Expression
type infixParseFn func(ast.Expression) ast.Expression

func NewParser(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.VOID, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_INT, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_STRING, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_BOOLEAN, p.parseFunctionLiteral)
	p.registerPrefix(token.TYPE_ANY, p.parseFunctionLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)
	p.registerPrefix(token.NEW, p.parseNewExpression)
	p.registerPrefix(token.THIS, p.parseThisExpression)

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
	p.registerInfix(token.DOT, p.parseDotExpression)
	p.registerInfix(token.ASSIGN, p.parseAssignmentExpression)

	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("Se esperaba el siguiente token de tipo %s, pero se obtuvo %s (%s)",
		t, p.peekToken.Type, p.peekToken.Literal)
	p.errors = append(p.errors, msg)
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) currentTokenIs(t token.TokenType) bool {
	return p.currentToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) currentPrecedence() int {
	if p, ok := precedences[p.currentToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

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

func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	case token.IF:
		return p.parseIfExpression()
	case token.CLASS:
		return p.parseClassStatement(nil)
	case token.PUBLIC, token.PRIVATE, token.PROTECTED:
		return p.parseStatementWithModifier()
	case token.LOG:
		return p.parseLogStatement()
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY:
		if p.peekTokenIs(token.IDENT) && p.isFunctionDeclarationAhead() {
			return p.parseFunctionDeclaration(p.currentToken)
		}
		return p.parseExpressionStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) isFunctionDeclarationAhead() bool {
	return false
}

func (p *Parser) parseStatementWithModifier() ast.Statement {
	modifier := p.currentToken
	p.nextToken()

	switch p.currentToken.Type {
	case token.CLASS:
		return p.parseClassStatement(&modifier)
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY:
		return p.parseFunctionDeclaration(modifier)
	default:
		p.errors = append(p.errors, fmt.Sprintf("Token inesperado '%s' después del modificador '%s'", p.currentToken.Literal, modifier.Literal))
		return nil
	}
}

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

func (p *Parser) peekTokenIsType() bool {
	switch p.peekToken.Type {
	case token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.VOID, token.TYPE_ANY, token.IDENT:
		return true
	default:
		return false
	}
}

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

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.currentToken}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}
	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}
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

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.currentToken, Value: p.currentToken.Literal}
}
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

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

func (p *Parser) parseAssignmentExpression(left ast.Expression) ast.Expression {
	exp := &ast.AssignmentExpression{
		Token: p.currentToken,
		Left:  left,
	}
	p.nextToken()
	exp.Value = p.parseExpression(LOWEST)
	return exp
}

func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.currentToken, Value: p.currentTokenIs(token.TRUE)}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
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

func (p *Parser) parseFunctionDeclaration(initialToken token.Token) *ast.FunctionDeclaration {
	decl := &ast.FunctionDeclaration{Token: initialToken, ReturnType: &ast.Identifier{Token: initialToken, Value: initialToken.Literal}}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	decl.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	decl.Parameters = p.parseFunctionParameters()

	if p.peekTokenIs(token.COLON) {
		p.nextToken()
		if !p.peekTokenIsType() {
			p.peekError(token.IDENT)
			return nil
		}
		p.nextToken()
		decl.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	decl.Body = p.parseBlockStatement()

	return decl
}

func (p *Parser) parseFunctionParameters() []*ast.Parameter {
	parameters := []*ast.Parameter{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return parameters
	}

	p.nextToken()

	param := &ast.Parameter{}
	if p.currentTokenIs(token.IDENT) {
		param.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

		if p.peekTokenIs(token.COLON) {
			p.nextToken()
			if !p.peekTokenIsType() {
				p.peekError(token.IDENT)
				return nil
			}
			p.nextToken()
			param.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
		}
	} else {
		p.errors = append(p.errors, fmt.Sprintf("Se esperaba un identificador para el parámetro, pero se obtuvo %s (%s)", p.currentToken.Type, p.currentToken.Literal))
		return nil
	}
	parameters = append(parameters, param)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()

		param = &ast.Parameter{}
		if p.currentTokenIs(token.IDENT) {
			param.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

			if p.peekTokenIs(token.COLON) {
				p.nextToken()
				if !p.peekTokenIsType() {
					p.peekError(token.IDENT)
					return nil
				}
				p.nextToken()
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

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.currentToken, Function: function}
	exp.Arguments = p.parseCallArguments()
	return exp
}

func (p *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken()
	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return args
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		if !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
			p.nextToken()
		}
	}
	return block
}

func (p *Parser) parseIfExpression() ast.Statement {
	expression := &ast.IfExpression{Token: p.currentToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if !p.expectPeek(token.LBRACE) {
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

func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.currentToken}
	array.Elements = p.parseExpressionList(token.RBRACKET)
	return array
}

func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()

	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.currentToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

func (p *Parser) parseModuleStatement() *ast.ModuleStatement {
	stmt := &ast.ModuleStatement{Token: p.currentToken}

	p.nextToken()

	switch p.currentToken.Type {
	case token.LET:
		letStmt := p.parseLetStatement()
		if letStmt == nil {
			return nil
		}
		stmt.Value = letStmt
		stmt.Name = letStmt.Name
	case token.VOID, token.TYPE_INT, token.TYPE_STRING, token.TYPE_BOOLEAN, token.TYPE_ANY:
		if p.peekTokenIs(token.IDENT) {
			funcDecl := p.parseFunctionDeclaration(p.currentToken)
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
		classStmt := p.parseClassStatement(nil)
		if classStmt == nil {
			return nil
		}
		stmt.Value = classStmt
		stmt.Name = classStmt.Name
	default:
		p.errors = append(p.errors, fmt.Sprintf("Tipo de declaración no soportado para 'module': %s", p.currentToken.Type))
		return nil
	}

	_, isLet := stmt.Value.(*ast.LetStatement)
	if isLet && p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseClassStatement(modifier *token.Token) *ast.ClassStatement {
	stmt := &ast.ClassStatement{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if p.peekTokenIs(token.EXTENDS) {
		p.nextToken()
		if !p.expectPeek(token.IDENT) {
			return nil
		}
		stmt.SuperClass = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	stmt.Body = p.parseClassBody(stmt.Name.Value)

	return stmt
}

func (p *Parser) parseClassBody(className string) *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
		var member ast.Statement = nil

		if p.currentTokenIs(token.IDENT) && p.currentToken.Literal == className && p.peekTokenIs(token.LPAREN) {
			member = p.parseMethodDeclaration(true)
		} else if p.currentTokenIs(token.VOID) || p.currentTokenIsType() {

			if p.peekTokenIs(token.IDENT) {
				member = p.parseMethodDeclaration(false)
			} else {

				p.errors = append(p.errors, fmt.Sprintf("Declaración de método o expresión inesperada en el cuerpo de la clase: %s (%s)", p.currentToken.Type, p.currentToken.Literal))
				p.nextToken()
				continue
			}
		} else if p.currentTokenIs(token.LET) {
			member = p.parsePropertyDeclaration()
		} else {
			p.errors = append(p.errors, fmt.Sprintf("Token inesperado en el cuerpo de la clase: %s (%s)", p.currentToken.Type, p.currentToken.Literal))
			p.nextToken()
			continue
		}

		if member != nil {
			block.Statements = append(block.Statements, member)
		}

		if !p.currentTokenIs(token.RBRACE) && !p.currentTokenIs(token.EOF) {
			p.nextToken()
		}
	}
	return block
}

func (p *Parser) parseMethodDeclaration(isConstructor bool) *ast.MethodDeclaration {
	method := &ast.MethodDeclaration{Token: p.currentToken, IsConstructor: isConstructor}

	if !isConstructor {

		if !p.currentTokenIsType() {
			p.errors = append(p.errors, fmt.Sprintf("Se esperaba un tipo de retorno para el método, pero se obtuvo %s (%s)", p.currentToken.Type, p.currentToken.Literal))
			return nil
		}
		method.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

		if !p.expectPeek(token.IDENT) {
			return nil
		}
	} else {
		method.ReturnType = nil
	}

	method.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	method.Parameters = p.parseFunctionParameters()

	if p.peekTokenIs(token.COLON) && !isConstructor {
		p.nextToken()
		if !p.peekTokenIsType() {
			p.peekError(token.IDENT)
			return nil
		}
		p.nextToken()
		method.ReturnType = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	method.Body = p.parseBlockStatement()

	return method
}

func (p *Parser) parsePropertyDeclaration() *ast.PropertyDeclaration {
	prop := &ast.PropertyDeclaration{Token: p.currentToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	prop.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if p.peekTokenIs(token.COLON) {
		p.nextToken()
		if !p.peekTokenIsType() {
			p.peekError(token.IDENT)
			return nil
		}
		p.nextToken()
		prop.TypeAnnotation = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	}

	if p.peekTokenIs(token.ASSIGN) {
		p.nextToken()
		prop.Value = p.parseExpression(LOWEST)
	}

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	} else {
		p.errors = append(p.errors, fmt.Sprintf("Se esperaba ';' después de la declaración de propiedad, pero se obtuvo %s (%s)", p.peekToken.Type, p.peekToken.Literal))
	}

	return prop
}

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
		p.nextToken()
		leftExp = infix(leftExp)
	}
	return leftExp
}

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
	if !p.expectPeek(token.IDENT) {
		return nil
	}
	exp.Class = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	exp.Arguments = p.parseCallArguments()
	return exp
}

func (p *Parser) parseThisExpression() ast.Expression {
	return &ast.ThisExpression{Token: p.currentToken}
}
