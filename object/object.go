package object

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/atrox39/lambda/ast"
	"github.com/atrox39/lambda/token"
)

type ObjectType string
type AccessModifier int

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
	CLASS_OBJ        = "CLASS"
	INSTANCE_OBJ     = "INSTANCE"
	BOUND_METHOD_OBJ = "BOUND_METHOD"
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

type Function struct {
	Parameters []*ast.Parameter
	Body       *ast.BlockStatement
	Env        *Environment
	Name       string
	IsStatic   bool
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

	params := []string{}
	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	if f.Body != nil {
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

type BuiltinFunction func(env *Environment, args ...Object) Object
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

type ClassProperty struct {
	Name           string
	DefaultValue   ast.Expression
	TypeAnnotation *ast.Identifier
	Modifier       AccessModifier
	IsStatic       bool
}

type ClassMethod struct {
	Name          string
	Function      *Function
	Modifier      AccessModifier
	IsConstructor bool
	IsStatic      bool
}

type Class struct {
	Name            *ast.Identifier
	InstanceMethods map[string]*ClassMethod
	InstanceProps   map[string]*ClassProperty

	StaticEnv *Environment

	SuperClass    *Class
	DefinitionEnv *Environment
}

func (c *Class) Type() ObjectType { return CLASS_OBJ }
func (c *Class) Inspect() string {
	var out bytes.Buffer
	out.WriteString("class ")
	out.WriteString(c.Name.Value)
	out.WriteString(" { ... }")
	return out.String()
}

type Instance struct {
	Class *Class
	Env   *Environment
}

func (i *Instance) Type() ObjectType { return INSTANCE_OBJ }
func (i *Instance) Inspect() string {
	return fmt.Sprintf("%s instance", i.Class.Name.Value)
}

type BoundMethod struct {
	Method   *Function
	Env      *Environment
	Instance *Instance
}

func (bm *BoundMethod) Type() ObjectType { return BOUND_METHOD_OBJ }
func (bm *BoundMethod) Inspect() string {
	return fmt.Sprintf("method[%s.%s]", bm.Instance.Class.Name.Value, bm.Method.Name)
}

func ModifierFromToken(tok token.Token) AccessModifier {
	switch tok.Type {
	case token.PRIVATE:
		return Private
	case token.PROTECTED:
		return Protected
	case token.PUBLIC:
		return Public
	default:
		return Public
	}
}
