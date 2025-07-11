package ast

import (
	"strings"

	"github.com/atrox39/lambda/token"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out strings.Builder
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

type LetStatement struct {
	Token          token.Token
	Name           *Identifier
	TypeAnnotation *Identifier
	Value          Expression
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }
func (ls *LetStatement) String() string {
	var out strings.Builder
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	if ls.TypeAnnotation != nil {
		out.WriteString(": " + ls.TypeAnnotation.String())
	}
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	var out strings.Builder
	out.WriteString(rs.TokenLiteral() + " ")
	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}
	out.WriteString(";")
	return out.String()
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

type Identifier struct {
	Token token.Token
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + pe.Operator)
	if pe.Right != nil {
		out.WriteString(pe.Right.String())
	}
	out.WriteString(")")
	return out.String()
}

type InfixExpression struct {
	Token    token.Token
	Left     Expression
	Operator string
	Right    Expression
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ie.Left.String() + " " + ie.Operator + " " + ie.Right.String() + ")")
	return out.String()
}

type AssignmentExpression struct {
	Token token.Token
	Left  Expression
	Value Expression
}

func (ae *AssignmentExpression) expressionNode()      {}
func (ae *AssignmentExpression) TokenLiteral() string { return ae.Token.Literal }
func (ae *AssignmentExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ae.Left.String() + " = " + ae.Value.String() + ")")
	return out.String()
}

type Boolean struct {
	Token token.Token
	Value bool
}

func (b *Boolean) expressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string       { return b.Token.Literal }

type Parameter struct {
	Name           *Identifier
	TypeAnnotation *Identifier
}

func (p *Parameter) String() string {
	var out strings.Builder
	out.WriteString(p.Name.String())
	if p.TypeAnnotation != nil {
		out.WriteString(": " + p.TypeAnnotation.String())
	}
	return out.String()
}

type FunctionLiteral struct {
	Token      token.Token
	ReturnType *Identifier
	Parameters []*Parameter
	Body       *BlockStatement
}

func (fl *FunctionLiteral) expressionNode()      {}
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FunctionLiteral) String() string {
	var out strings.Builder
	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	if fl.ReturnType != nil {
		out.WriteString(": " + fl.ReturnType.String())
	}
	if fl.Body != nil {
		out.WriteString(fl.Body.String())
	}
	return out.String()
}

type FunctionDeclaration struct {
	Token      token.Token
	ReturnType *Identifier
	Name       *Identifier
	Parameters []*Parameter
	Body       *BlockStatement
}

func (fd *FunctionDeclaration) statementNode()       {}
func (fd *FunctionDeclaration) TokenLiteral() string { return fd.Token.Literal }
func (fd *FunctionDeclaration) String() string {
	var out strings.Builder
	if fd.ReturnType != nil {
		out.WriteString(fd.ReturnType.String() + " ")
	}
	out.WriteString(fd.Name.String() + "(")
	params := []string{}
	for _, p := range fd.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	if fd.ReturnType != nil {
		out.WriteString(": " + fd.ReturnType.String())
	}
	if fd.Body != nil {
		out.WriteString(fd.Body.String())
	}
	return out.String()
}

type CallExpression struct {
	Token     token.Token
	Function  Expression
	Arguments []Expression
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CallExpression) String() string {
	var out strings.Builder
	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}
	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

type BlockStatement struct {
	Token      token.Token
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out strings.Builder
	out.WriteString("{")
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	out.WriteString("}")
	return out.String()
}

type IfExpression struct {
	Token       token.Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) statementNode()       {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) String() string {
	var out strings.Builder
	out.WriteString("if" + ie.Condition.String() + " " + ie.Consequence.String())
	if ie.Alternative != nil {
		out.WriteString("else " + ie.Alternative.String())
	}
	return out.String()
}

type LogStatement struct {
	Token    token.Token
	Argument Expression
}

func (ls *LogStatement) statementNode()       {}
func (ls *LogStatement) TokenLiteral() string { return ls.Token.Literal }
func (ls *LogStatement) String() string {
	var out strings.Builder
	out.WriteString("log(")
	if ls.Argument != nil {
		out.WriteString(ls.Argument.String())
	}
	out.WriteString(");")
	return out.String()
}

type ArrayLiteral struct {
	Token    token.Token
	Elements []Expression
}

func (al *ArrayLiteral) expressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
func (al *ArrayLiteral) String() string {
	var out strings.Builder
	elements := []string{}
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

type IndexExpression struct {
	Token token.Token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ie.Left.String() + "[" + ie.Index.String() + "])")
	return out.String()
}

type DotExpression struct {
	Token  token.Token
	Left   Expression
	Member *Identifier
}

func (de *DotExpression) expressionNode()      {}
func (de *DotExpression) TokenLiteral() string { return de.Token.Literal }
func (de *DotExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + de.Left.String() + "." + de.Member.String() + ")")
	return out.String()
}

type ModuleStatement struct {
	Token token.Token
	Name  *Identifier
	Value Statement
}

func (ms *ModuleStatement) statementNode()       {}
func (ms *ModuleStatement) TokenLiteral() string { return ms.Token.Literal }
func (ms *ModuleStatement) String() string {
	var out strings.Builder
	out.WriteString(ms.TokenLiteral() + " ")
	if ms.Name != nil {
		out.WriteString(ms.Name.String() + " ")
	}
	if ms.Value != nil {
		out.WriteString(ms.Value.String())
	}
	return out.String()
}

type ClassStatement struct {
	Token      token.Token
	Name       *Identifier
	SuperClass *Identifier
	Body       *BlockStatement
}

func (cs *ClassStatement) statementNode()       {}
func (cs *ClassStatement) TokenLiteral() string { return cs.Token.Literal }
func (cs *ClassStatement) String() string {
	var out strings.Builder
	out.WriteString(cs.TokenLiteral() + " " + cs.Name.String())
	if cs.SuperClass != nil {
		out.WriteString(" extends " + cs.SuperClass.String())
	}
	out.WriteString(" ")
	if cs.Body != nil {
		out.WriteString(cs.Body.String())
	} else {
		out.WriteString("{}")
	}
	return out.String()
}

type PropertyDeclaration struct {
	Token          token.Token
	Modifier       token.Token
	IsStatic       bool
	Name           *Identifier
	TypeAnnotation *Identifier
	Value          Expression
}

func (pd *PropertyDeclaration) statementNode()       {}
func (pd *PropertyDeclaration) TokenLiteral() string { return pd.Modifier.Literal }
func (pd *PropertyDeclaration) String() string {
	var out strings.Builder
	if pd.Modifier.Type == token.PUBLIC || pd.Modifier.Type == token.PRIVATE || pd.Modifier.Type == token.PROTECTED {
		out.WriteString(pd.Modifier.Literal + " ")
	}
	if pd.IsStatic {
		out.WriteString("static ")
	}

	out.WriteString(pd.Name.String())
	if pd.TypeAnnotation != nil {
		out.WriteString(": " + pd.TypeAnnotation.String())
	}
	if pd.Value != nil {
		out.WriteString(" = " + pd.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

type MethodDeclaration struct {
	Token         token.Token
	Modifier      token.Token
	IsStatic      bool
	ReturnType    *Identifier
	Name          *Identifier
	Parameters    []*Parameter
	Body          *BlockStatement
	IsConstructor bool
}

func (md *MethodDeclaration) statementNode()       {}
func (md *MethodDeclaration) TokenLiteral() string { return md.Modifier.Literal }
func (md *MethodDeclaration) String() string {
	var out strings.Builder
	if md.Modifier.Type == token.PUBLIC || md.Modifier.Type == token.PRIVATE || md.Modifier.Type == token.PROTECTED {
		out.WriteString(md.Modifier.Literal + " ")
	}
	if md.IsStatic {
		out.WriteString("static ")
	}

	if !md.IsConstructor && md.ReturnType != nil {
		out.WriteString(md.ReturnType.String() + " ")
	}
	out.WriteString(md.Name.String())
	out.WriteString("(")
	params := []string{}
	for _, p := range md.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	if md.Body != nil {
		out.WriteString(" " + md.Body.String())
	} else {
		out.WriteString(" {}")
	}
	return out.String()
}

type NewExpression struct {
	Token     token.Token
	Class     *Identifier
	Arguments []Expression
}

func (ne *NewExpression) expressionNode()      {}
func (ne *NewExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NewExpression) String() string {
	var out strings.Builder
	args := []string{}
	for _, a := range ne.Arguments {
		args = append(args, a.String())
	}
	out.WriteString("new " + ne.Class.String() + "(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

type ThisExpression struct {
	Token token.Token
}

func (te *ThisExpression) expressionNode()      {}
func (te *ThisExpression) TokenLiteral() string { return te.Token.Literal }
func (te *ThisExpression) String() string       { return "this" }
