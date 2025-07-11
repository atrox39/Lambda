// ast/ast.go
package ast

import (
	"strings"

	"github.com/atrox39/lambda/token"
)

// Node es la interfaz base para todos los nodos del AST.
type Node interface {
	TokenLiteral() string // Devuelve el literal del token asociado al nodo.
	String() string       // Devuelve una representación en cadena del nodo (para depuración).
}

// Statement es la interfaz para las declaraciones (ej. let x = 5;).
type Statement interface {
	Node
	statementNode() // Método marcador para diferenciar declaraciones.
}

// Expression es la interfaz para las expresiones (ej. 5 + 5; x * y;).
type Expression interface {
	Node
	expressionNode() // Método marcador para diferenciar expresiones.
}

// Program es el nodo raíz de cada AST.
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

// LetStatement representa una declaración 'let' (ej. let x = 5; let y: int = 10;).
type LetStatement struct {
	Token          token.Token // El token LET
	Name           *Identifier // El identificador (nombre de la variable)
	TypeAnnotation *Identifier // Opcional: la anotación de tipo (ej. "int")
	Value          Expression  // La expresión que produce el valor asignado
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

// ReturnStatement representa una declaración 'return' (ej. return 10;).
type ReturnStatement struct {
	Token       token.Token // El token RETURN
	ReturnValue Expression  // El valor a retornar
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

// ExpressionStatement representa una expresión usada como declaración (ej. x + y;).
type ExpressionStatement struct {
	Token      token.Token // El primer token de la expresión
	Expression Expression  // La expresión
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// Identifier representa un identificador (ej. x, miVariable, funcionPrincipal).
type Identifier struct {
	Token token.Token // El token IDENT
	Value string      // El valor del identificador
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

// IntegerLiteral representa un literal entero (ej. 5, 100).
type IntegerLiteral struct {
	Token token.Token // El token INT
	Value int64       // El valor entero
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

// StringLiteral representa un literal de cadena (ej. "hola mundo").
type StringLiteral struct {
	Token token.Token // El token STRING
	Value string      // El valor de la cadena
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

// PrefixExpression representa una expresión prefijo (ej. !true, -5).
type PrefixExpression struct {
	Token    token.Token // El token del operador prefijo (ej. BANG, MINUS)
	Operator string      // El operador (ej. "!", "-")
	Right    Expression  // La expresión a la derecha del operador
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

// InfixExpression representa una expresión infijo (ej. 5 + 5, x == y).
type InfixExpression struct {
	Token    token.Token // El token del operador infijo (ej. PLUS, EQ)
	Left     Expression  // La expresión a la izquierda del operador
	Operator string      // El operador (ej. "+", "==")
	Right    Expression  // La expresión a la derecha del operador
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ie.Left.String() + " " + ie.Operator + " " + ie.Right.String() + ")")
	return out.String()
}

// AssignmentExpression representa una asignación (ej. x = 5; this.prop = val;).
type AssignmentExpression struct {
	Token token.Token // El ASSIGN token
	Left  Expression  // La expresión siendo asignada a (ej. Identifier, DotExpression, IndexExpression)
	Value Expression  // El valor siendo asignado
}

func (ae *AssignmentExpression) expressionNode()      {}
func (ae *AssignmentExpression) TokenLiteral() string { return ae.Token.Literal }
func (ae *AssignmentExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ae.Left.String() + " = " + ae.Value.String() + ")")
	return out.String()
}

// Boolean representa un literal booleano (true/false).
type Boolean struct {
	Token token.Token // El token TRUE o FALSE
	Value bool        // El valor booleano
}

func (b *Boolean) expressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string       { return b.Token.Literal }

// Parameter representa un parámetro de función o método con su tipo.
type Parameter struct {
	Name           *Identifier // El nombre del parámetro
	TypeAnnotation *Identifier // La anotación de tipo del parámetro
}

func (p *Parameter) String() string {
	var out strings.Builder
	out.WriteString(p.Name.String()) // Nombre del parámetro
	if p.TypeAnnotation != nil {
		out.WriteString(": " + p.TypeAnnotation.String()) // Anotación de tipo
	}
	return out.String()
}

// FunctionLiteral representa una expresión de función (ej. void(int x, string y) { ... }).
type FunctionLiteral struct {
	Token      token.Token     // El token VOID (que también es el tipo de retorno)
	ReturnType *Identifier     // El tipo de retorno explícito (ej. "void", "int")
	Parameters []*Parameter    // Los parámetros de la función con sus tipos
	Body       *BlockStatement // El cuerpo de la función
}

func (fl *FunctionLiteral) expressionNode()      {}
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FunctionLiteral) String() string {
	var out strings.Builder
	out.WriteString(fl.TokenLiteral()) // "void"
	out.WriteString("(")
	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	if fl.ReturnType != nil {
		out.WriteString(": " + fl.ReturnType.String()) // Tipo de retorno
	}
	if fl.Body != nil {
		out.WriteString(fl.Body.String())
	}
	return out.String()
}

// FunctionDeclaration representa una declaración de función (e.g., void funcName(int a, string b): int { ... }).
type FunctionDeclaration struct {
	Token      token.Token     // El VOID token (o el token del tipo de retorno)
	ReturnType *Identifier     // El tipo de retorno explícito (ej. "void", "int")
	Name       *Identifier     // El nombre de la función
	Parameters []*Parameter    // Los parámetros de la función con sus tipos
	Body       *BlockStatement // El cuerpo de la función
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
	if fd.ReturnType != nil { // Vuelve a añadir el tipo de retorno después de los paréntesis
		out.WriteString(": " + fd.ReturnType.String())
	}
	if fd.Body != nil {
		out.WriteString(fd.Body.String())
	}
	return out.String()
}

// CallExpression representa una llamada a función (ej. add(2, 3)).
type CallExpression struct {
	Token     token.Token  // El paréntesis izquierdo LPAREN
	Function  Expression   // El identificador o literal de función
	Arguments []Expression // Los argumentos de la llamada
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

// BlockStatement representa un bloque de código (ej. { ... }).
// También se usará para el cuerpo de la clase.
type BlockStatement struct {
	Token      token.Token // El token LBRACE
	Statements []Statement // Las declaraciones dentro del bloque
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

// IfExpression representa una expresión 'if' (ej. if (x > 0) { ... } else { ... }).
type IfExpression struct {
	Token       token.Token     // El token IF
	Condition   Expression      // La condición (expresión booleana)
	Consequence *BlockStatement // El bloque de código si la condición es verdadera
	Alternative *BlockStatement // El bloque de código si la condición es falsa (opcional)
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

// LogStatement representa una declaración 'log' (ej. log("hola");).
type LogStatement struct {
	Token    token.Token // El token LOG
	Argument Expression  // El argumento a imprimir
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

// ArrayLiteral representa un literal de array (ej. [1, 2, "hola"]).
type ArrayLiteral struct {
	Token    token.Token  // El token LBRACKET
	Elements []Expression // Los elementos del array
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

// IndexExpression representa una expresión de índice (ej. miArray[0]).
type IndexExpression struct {
	Token token.Token // El token LBRACKET
	Left  Expression  // La expresión del array/objeto
	Index Expression  // La expresión del índice
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + ie.Left.String() + "[" + ie.Index.String() + "])")
	return out.String()
}

// DotExpression representa una expresión de acceso a miembro (ej. objeto.propiedad, objeto.metodo()).
type DotExpression struct {
	Token  token.Token // El token DOT
	Left   Expression  // La expresión del objeto/instancia
	Member *Identifier // El identificador del miembro (propiedad o método)
}

func (de *DotExpression) expressionNode()      {}
func (de *DotExpression) TokenLiteral() string { return de.Token.Literal }
func (de *DotExpression) String() string {
	var out strings.Builder
	out.WriteString("(" + de.Left.String() + "." + de.Member.String() + ")")
	return out.String()
}

// ModuleStatement representa una declaración 'module' (ej. module let x = 10;).
type ModuleStatement struct {
	Token token.Token // El token MODULE
	Name  *Identifier // El identificador (nombre del módulo/exportación si aplica)
	Value Statement   // La declaración que se está exportando (LetStatement, FunctionDeclaration, ClassStatement, etc.)
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

// --- Nuevos Nodos AST para Clases ---

// ClassStatement representa una declaración de clase.
type ClassStatement struct {
	Token      token.Token     // El CLASS token
	Name       *Identifier     // El nombre de la clase
	SuperClass *Identifier     // Opcional: El nombre de la superclase si 'extends' es usado
	Body       *BlockStatement // El cuerpo de la clase, conteniendo PropertyDeclaration y MethodDeclaration
}

func (cs *ClassStatement) statementNode()       {}
func (cs *ClassStatement) TokenLiteral() string { return cs.Token.Literal }
func (cs *ClassStatement) String() string {
	var out strings.Builder
	out.WriteString(cs.TokenLiteral() + " " + cs.Name.String())
	if cs.SuperClass != nil {
		out.WriteString(" extends " + cs.SuperClass.String())
	}
	out.WriteString(" ") // Espacio antes del cuerpo
	if cs.Body != nil {   // El cuerpo es un BlockStatement que ya incluye { }
		out.WriteString(cs.Body.String())
	} else {
		out.WriteString("{}") // Si el cuerpo está vacío
	}
	return out.String()
}

// PropertyDeclaration representa una propiedad dentro de una clase.
type PropertyDeclaration struct {
	Token          token.Token // El token del modificador (PUBLIC, PRIVATE, PROTECTED) o LET si es implícito público
	Modifier       token.Token // Guardamos el token del modificador explícito
	IsStatic       bool        // Nuevo: true si la propiedad es estática
	Name           *Identifier // El nombre de la propiedad
	TypeAnnotation *Identifier // Opcional: la anotación de tipo (ej. "int")
	Value          Expression  // Opcional: El valor inicial de la propiedad
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
	// Si es público implícito (Modifier no es P/P/P y Token original era LET), no se imprime "let"
    // Esta lógica de impresión de "let" es compleja si Modifier puede ser LET.
    // Asumimos que si Modifier es LET, es porque no hubo public/private/protected explícito.
    // Y si es LET y no es estático, es una propiedad de instancia pública implícita.
    // Si es LET y ESTÁTICO, la sintaxis sería "static let", lo cual es redundante si static implica let.
    // Por ahora, si `IsStatic` es true, imprimimos "static". Si hay Modifier explícito, se imprime.
    // El "let" implícito para propiedades de instancia no estáticas no se imprime.

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

// MethodDeclaration representa un método dentro de una clase.
type MethodDeclaration struct {
	Token         token.Token     // El token del modificador (PUBLIC, PRIVATE, PROTECTED) o el tipo de retorno si es implícito público
	Modifier      token.Token     // Guardamos el token del modificador explícito
	IsStatic      bool            // Nuevo: true si el método es estático
	ReturnType    *Identifier     // El tipo de retorno explícito (ej. "void", "int")
	Name          *Identifier     // El nombre del método (o clase para constructor)
	Parameters    []*Parameter    // Los parámetros del método
	Body          *BlockStatement // El cuerpo del método
	IsConstructor bool            // True si este método es el constructor
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
	// El tipo de retorno para constructores no se imprime.
	// Para métodos, si hay un tipo de retorno explícito después de los parámetros (como en TypeScript), se añadiría aquí.
	// Por ahora, el diseño lo pone antes del nombre del método.
	if md.Body != nil { // El cuerpo es un BlockStatement que ya incluye { }
		out.WriteString(" " + md.Body.String())
	} else {
		out.WriteString(" {}") // Si el cuerpo está vacío (no debería pasar para métodos bien formados)
	}
	return out.String()
}

// NewExpression representa una expresión 'new' (ej. new ClassName(args)).
type NewExpression struct {
	Token     token.Token  // El token NEW
	Class     *Identifier  // El identificador de la clase
	Arguments []Expression // Los argumentos del constructor
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

// ThisExpression representa la palabra clave 'this'.
// Será parseado como un Identifier con valor "this", pero podemos tener un nodo específico si es necesario
// por ahora, lo manejaremos como un Identifier especial en el evaluador.
// Para mantener la consistencia con el diseño, lo añadimos aunque el parser lo genere como Identifier.
type ThisExpression struct {
	Token token.Token // El token THIS
}

func (te *ThisExpression) expressionNode()      {}
func (te *ThisExpression) TokenLiteral() string { return te.Token.Literal }
func (te *ThisExpression) String() string       { return "this" }