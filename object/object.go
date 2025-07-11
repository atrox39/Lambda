// object/object.go
package object

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/atrox39/lambda/ast"
	"github.com/atrox39/lambda/token" // Necesario para AccessModifier
)

type ObjectType string
type AccessModifier int // Nuevo tipo para modificadores de acceso

// Constantes para Modificadores de Acceso
const (
	Public AccessModifier = iota
	Private
	Protected
)

const (
	INTEGER_OBJ      = "INTEGER"
	BOOLEAN_OBJ      = "BOOLEAN"
	NULL_OBJ         = "NULL"
	RETURN_VALUE_OBJ = "RETURN_VALUE"
	ERROR_OBJ        = "ERROR"
	FUNCTION_OBJ     = "FUNCTION"
	STRING_OBJ       = "STRING"
	BUILTIN_OBJ      = "BUILTIN"
	ARRAY_OBJ        = "ARRAY"
	CLASS_OBJ        = "CLASS"   // Nuevo tipo de objeto para Clases
	INSTANCE_OBJ     = "INSTANCE" // Nuevo tipo de objeto para Instancias de Clases
	BOUND_METHOD_OBJ = "BOUND_METHOD" // Nuevo
)

type Object interface {
	Type() ObjectType
	Inspect() string
}

type Integer struct {
	Value int64
}

func (i *Integer) Type() ObjectType { return INTEGER_OBJ }
func (i *Integer) Inspect() string  { return fmt.Sprintf("%d", i.Value) }

type Boolean struct {
	Value bool
}

func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }
func (b *Boolean) Inspect() string  { return fmt.Sprintf("%t", b.Value) }

type Null struct{}

func (n *Null) Type() ObjectType { return NULL_OBJ }
func (n *Null) Inspect() string  { return "null" }

type ReturnValue struct {
	Value Object
}

func (rv *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }
func (rv *ReturnValue) Inspect() string  { return rv.Value.Inspect() }

type Error struct {
	Message string
}

func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect() string  { return "ERROR: " + e.Message }

type Environment struct {
	store map[string]Object
	outer *Environment
	// Podríamos añadir aquí una referencia a la instancia actual para `this`,
	// o pasar `this` explícitamente durante la evaluación de métodos.
	// Por ahora, el evaluador manejará `this`.
}

func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s, outer: nil}
}

func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}

// Function representa una función definida por el usuario o un método.
type Function struct {
	Parameters []*ast.Parameter
	Body       *ast.BlockStatement
	Env        *Environment // Entorno donde la función/método fue definida
	Name       string // Nombre de la función/método para debugging o referencia
	IsStatic   bool   // Nuevo: Indica si es un método estático
}

func (f *Function) Type() ObjectType { return FUNCTION_OBJ }
func (f *Function) Inspect() string {
	var out strings.Builder
	if f.IsStatic {
		out.WriteString("static ")
	}
	out.WriteString("fn ")
	if f.Name != "" {
		out.WriteString(f.Name)
	}
	var out strings.Builder

	params := []string{}
	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	if f.Body != nil { // El cuerpo puede ser nil para builtins o interfaces futuras
		out.WriteString(f.Body.String())
	}
	out.WriteString("\n}")
	return out.String()
}

type String struct {
	Value string
}

func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect() string  { return s.Value }

type BuiltinFunction func(env *Environment, args ...Object) Object // Modificado para pasar el entorno actual

type Builtin struct {
	Fn BuiltinFunction
}

func (b *Builtin) Type() ObjectType { return BUILTIN_OBJ }
func (b *Builtin) Inspect() string  { return "builtin function" }

type Array struct {
	Elements []Object
}

func (ao *Array) Type() ObjectType { return ARRAY_OBJ }
func (ao *Array) Inspect() string {
	var out strings.Builder
	elements := []string{}
	for _, e := range ao.Elements {
		elements = append(elements, e.Inspect())
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

// --- Nuevos Objetos para Clases ---

// ClassProperty define una propiedad de una clase.
type ClassProperty struct {
	Name           string
	DefaultValue   ast.Expression
	TypeAnnotation *ast.Identifier
	Modifier       AccessModifier
	IsStatic       bool // Ya estaba en el AST, útil aquí también para la definición.
}

// ClassMethod define un método de una clase (tanto instancia como estático en su definición).
type ClassMethod struct {
	Name          string
	Function      *Function
	Modifier      AccessModifier
	IsConstructor bool
	IsStatic      bool // Ya estaba en el AST.
}

// Class representa la definición de una clase en tiempo de ejecución.
type Class struct {
	Name            *ast.Identifier
	InstanceMethods map[string]*ClassMethod   // Métodos de instancia
	InstanceProps   map[string]*ClassProperty // Definiciones de propiedades de instancia

	StaticEnv       *Environment              // Nuevo: Almacena valores de props estáticas y Funciones de métodos estáticos
	                                          // Los métodos estáticos (object.Function) tendrán este StaticEnv como su f.Env.
	                                          // Las propiedades estáticas (object.Object) se almacenarán directamente aquí.

	SuperClass      *Class
	DefinitionEnv   *Environment              // Entorno donde la 'class Foo {}' fue declarada (para closure de la clase)
}

func (c *Class) Type() ObjectType { return CLASS_OBJ }
func (c *Class) Inspect() string {
	var out bytes.Buffer
	out.WriteString("class ")
	out.WriteString(c.Name.Value)
	out.WriteString(" { ... }") // Podríamos añadir más detalles si es necesario
	return out.String()
}

// Instance representa una instancia de una clase.
type Instance struct {
	Class *Class       // La clase de la cual esta es una instancia
	Env   *Environment // Entorno para las propiedades de esta instancia específica
}

func (i *Instance) Type() ObjectType { return INSTANCE_OBJ }
func (i *Instance) Inspect() string {
	// Podríamos intentar mostrar las propiedades, pero puede ser complejo/verboso.
	// Por ahora, solo el nombre de la clase.
	return fmt.Sprintf("%s instance", i.Class.Name.Value)
}

// BoundMethod representa un método que ha sido vinculado a una instancia específica ('this').
type BoundMethod struct {
	Method   *Function    // La definición original de la función del método
	Env      *Environment // El entorno para ejecutar el método (contiene 'this' y está enlazado al entorno de la clase)
	Instance *Instance    // La instancia a la que está vinculado este método
}

func (bm *BoundMethod) Type() ObjectType { return BOUND_METHOD_OBJ }
func (bm *BoundMethod) Inspect() string {
	// Podríamos mostrar bm.Instance.Class.Name.Value + "." + bm.Method.Name
	return fmt.Sprintf("method[%s.%s]", bm.Instance.Class.Name.Value, bm.Method.Name)
}

// Helper para convertir token.TokenType a AccessModifier
func ModifierFromToken(tok token.Token) AccessModifier {
	switch tok.Type {
	case token.PRIVATE:
		return Private
	case token.PROTECTED:
		return Protected
	case token.PUBLIC:
		return Public
	default: // Si es LET o un tipo (para métodos implícitamente públicos)
		return Public // Por defecto es público
	}
}