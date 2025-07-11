// evaluator/evaluator.go
package evaluator

import (
	"fmt"

	"github.com/atrox39/lambda/ast"
	"github.com/atrox39/lambda/object"
	"github.com/atrox39/lambda/token"
)

var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

func Eval(node ast.Node, env *object.Environment) object.Object {
	switch node := node.(type) {
	case *ast.Program:
		return evalProgram(node, env)
	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)
	case *ast.BlockStatement:
		return evalBlockStatement(node, env)
	case *ast.ReturnStatement:
		val := Eval(node.ReturnValue, env)
		if isError(val) { return val }
		return &object.ReturnValue{Value: val}
	case *ast.LetStatement:
		val := Eval(node.Value, env)
		if isError(val) { return val }
		env.Set(node.Name.Value, val)
		return nil
	case *ast.LogStatement:
		val := Eval(node.Argument, env)
		if isError(val) { return val }
		fmt.Println(val.Inspect())
		return nil

	case *ast.ClassStatement:
		return evalClassStatement(node, env)

	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.StringLiteral:
		return &object.String{Value: node.Value}
	case *ast.Boolean:
		return nativeBoolToBooleanObject(node.Value)
	case *ast.PrefixExpression:
		right := Eval(node.Right, env)
		if isError(right) { return right }
		return evalPrefixExpression(node.Operator, right)
	case *ast.InfixExpression:
		left := Eval(node.Left, env)
		if isError(left) { return left }
		right := Eval(node.Right, env)
		if isError(right) { return right }
		return evalInfixExpression(node.Operator, left, right)
	case *ast.IfExpression:
		return evalIfExpression(node, env)
	case *ast.Identifier:
		return evalIdentifier(node, env)
	case *ast.FunctionLiteral:
		params := node.Parameters
		body := node.Body
		return &object.Function{Parameters: params, Env: env, Body: body, Name: "", IsStatic: false}
	case *ast.CallExpression:
		function := Eval(node.Function, env)
		if isError(function) { return function }
		args := evalExpressions(node.Arguments, env)
		if len(args) == 1 && isError(args[0]) { return args[0] }
		return applyFunction(function, args, env)
	case *ast.ArrayLiteral:
		elements := evalExpressions(node.Elements, env)
		if len(elements) == 1 && isError(elements[0]) { return elements[0] }
		return &object.Array{Elements: elements}
	case *ast.IndexExpression:
		left := Eval(node.Left, env)
		if isError(left) { return left }
		index := Eval(node.Index, env)
		if isError(index) { return index }
		return evalIndexExpression(left, index)
	case *ast.NewExpression:
		return evalNewExpression(node, env)
	case *ast.DotExpression:
		return evalDotExpression(node, env)
	case *ast.ThisExpression:
		return evalThisExpression(node, env)
	case *ast.AssignmentExpression:
		return evalAssignmentExpression(node, env)
	case *ast.PropertyDeclaration:
		return newError("Property declaration no es una expresión evaluable directamente.")
	case *ast.MethodDeclaration:
		return newError("Method declaration no es una expresión evaluable directamente.")
	}
	return newError("Evaluador no implementado para el nodo %T (%s)", node, node.String())
}

func evalProgram(program *ast.Program, env *object.Environment) object.Object {
	var result object.Object
	for _, statement := range program.Statements {
		result = Eval(statement, env)
		switch result := result.(type) {
		case *object.ReturnValue:
			return result.Value
		case *object.Error:
			return result
		}
	}
	return result
}

func evalBlockStatement(block *ast.BlockStatement, env *object.Environment) object.Object {
	var result object.Object
	for _, statement := range block.Statements {
		result = Eval(statement, env)
		if result != nil {
			rt := result.Type()
			if rt == object.RETURN_VALUE_OBJ || rt == object.ERROR_OBJ {
				return result
			}
		}
	}
	return result
}

func evalClassStatement(node *ast.ClassStatement, env *object.Environment) object.Object {
	className := node.Name

	staticEnv := object.NewEnclosedEnvironment(env)

	classObj := &object.Class{
		Name:            className,
		InstanceMethods: make(map[string]*object.ClassMethod),
		InstanceProps:   make(map[string]*object.ClassProperty),
		StaticEnv:       staticEnv,
		DefinitionEnv:   env,
	}

	env.Set(className.Value, classObj)

	if node.Body != nil {
		for _, stmtNode := range node.Body.Statements {
			switch memberNode := stmtNode.(type) {
			case *ast.PropertyDeclaration:
				propDef := &object.ClassProperty{
					Name:           memberNode.Name.Value,
					DefaultValue:   memberNode.Value,
					TypeAnnotation: memberNode.TypeAnnotation,
					Modifier:       object.ModifierFromToken(memberNode.Modifier),
					IsStatic:       memberNode.IsStatic,
				}
				if memberNode.IsStatic {
					if memberNode.Value != nil {
						val := Eval(memberNode.Value, classObj.StaticEnv)
						if isError(val) { return val }
						classObj.StaticEnv.Set(propDef.Name, val)
					} else {
						classObj.StaticEnv.Set(propDef.Name, NULL)
					}
				} else {
					classObj.InstanceProps[propDef.Name] = propDef
				}

			case *ast.MethodDeclaration:
				methodName := memberNode.Name.Value
				var methodDefiningEnv *object.Environment
				if memberNode.IsStatic {
					methodDefiningEnv = classObj.StaticEnv
				} else {
					methodDefiningEnv = classObj.DefinitionEnv
				}

				fn := &object.Function{
					Parameters: memberNode.Parameters,
					Body:       memberNode.Body,
					Env:        methodDefiningEnv,
					Name:       methodName,
					IsStatic:   memberNode.IsStatic,
				}
				classMethod := &object.ClassMethod{
					Name:          methodName,
					Function:      fn,
					Modifier:      object.ModifierFromToken(memberNode.Modifier),
					IsConstructor: memberNode.IsConstructor || (methodName == className.Value && !memberNode.IsStatic),
					IsStatic:      memberNode.IsStatic,
				}

				if memberNode.IsStatic {
					classObj.StaticEnv.Set(methodName, fn)
				} else {
					classObj.InstanceMethods[methodName] = classMethod
				}
			default:
				return newError("Tipo de declaración no esperado dentro del cuerpo de la clase: %T", memberNode)
			}
		}
	}
	return nil
}

func evalIdentifier(node *ast.Identifier, env *object.Environment) object.Object {
	if val, ok := env.Get(node.Value); ok {
		return val
	}
	if builtin, ok := builtins[node.Value]; ok {
		return builtin
	}
	return newError("Identificador no encontrado: %s", node.Value)
}

func evalNewExpression(node *ast.NewExpression, env *object.Environment) object.Object {
	classObj, ok := env.Get(node.Class.Value)
	if !ok {
		return newError("Clase no encontrada: %s", node.Class.Value)
	}

	class, ok := classObj.(*object.Class)
	if !ok {
		return newError("No es una clase: %s (tipo %s)", node.Class.Value, classObj.Type())
	}

	instance := &object.Instance{
		Class: class,
		Env:   object.NewEnclosedEnvironment(class.DefinitionEnv),
	}

	for propName, propDef := range class.InstanceProps {
		if propDef.DefaultValue != nil {
			propVal := Eval(propDef.DefaultValue, instance.Env)
			if isError(propVal) {
				return propVal
			}
			instance.Env.Set(propName, propVal)
		} else {
			instance.Env.Set(propName, NULL)
		}
	}

	constructorDef, constructorFound := class.InstanceMethods[class.Name.Value]

	args := evalExpressions(node.Arguments, env)
	if len(args) == 1 && isError(args[0]) {
		return args[0]
	}

	if constructorFound {
		if !constructorDef.IsConstructor {
			return newError("El método '%s' encontrado no está marcado como constructor.", class.Name.Value)
		}

		constructorCallEnv := object.NewEnclosedEnvironment(instance.Env)
		constructorCallEnv.Set("this", instance)

		if len(constructorDef.Function.Parameters) != len(args) {
			return newError("Constructor %s: número incorrecto de argumentos. Se esperaban %d, se obtuvieron %d",
				class.Name.Value, len(constructorDef.Function.Parameters), len(args))
		}
		for paramIdx, param := range constructorDef.Function.Parameters {
			constructorCallEnv.Set(param.Name.Value, args[paramIdx])
		}

		constructorResult := Eval(constructorDef.Function.Body, constructorCallEnv)
		if isError(constructorResult) {
			return constructorResult
		}
	} else {
		if len(args) > 0 {
			return newError("La clase '%s' no tiene constructor definido que acepte argumentos.", class.Name.Value)
		}
	}
	return instance
}

func evalDotExpression(node *ast.DotExpression, env *object.Environment) object.Object {
	left := Eval(node.Left, env)
	if isError(left) {
		return left
	}

	memberName := node.Member.Value

	switch obj := left.(type) {
	case *object.Instance:
		if val, ok := obj.Env.Get(memberName); ok {
			return val
		}
		if methodDef, ok := obj.Class.InstanceMethods[memberName]; ok {
			methodEnv := object.NewEnclosedEnvironment(methodDef.Function.Env)
			methodEnv.Set("this", obj)
			return &object.BoundMethod{
				Method:   methodDef.Function,
				Env:      methodEnv,
				Instance: obj,
			}
		}
		return newError("Miembro '%s' no encontrado en la instancia de '%s'", memberName, obj.Class.Name.Value)

	case *object.Class:
		if val, ok := obj.StaticEnv.Get(memberName); ok {
			return val
		}
		return newError("Miembro estático '%s' no encontrado en la clase '%s'", memberName, obj.Name.Value)

	default:
		return newError("El operador '.' solo se puede usar en instancias o clases (obtenido: %s)", left.Type())
	}
}

func evalThisExpression(node *ast.ThisExpression, env *object.Environment) object.Object {
	ifthisVal, ok := env.Get("this")
	if !ok {
		return newError("'this' no puede ser usado en este contexto.")
	}
	return ifthisVal
}

func evalAssignmentExpression(node *ast.AssignmentExpression, env *object.Environment) object.Object {
	val := Eval(node.Value, env)
	if isError(val) { return val }

	switch leftNode := node.Left.(type) {
	case *ast.Identifier:
		env.Set(leftNode.Value, val)
		return val

	case *ast.DotExpression:
		objLeft := Eval(leftNode.Left, env)
		if isError(objLeft) { return objLeft }

		memberName := leftNode.Member.Value

		if instance, okInstance := objLeft.(*object.Instance); okInstance {
			instance.Env.Set(memberName, val)
			return val
		} else if class, okClass := objLeft.(*object.Class); okClass {
			class.StaticEnv.Set(memberName, val)
			return val
		} else {
			return newError("Solo se puede asignar a propiedades de instancias o clases, se obtuvo %s", objLeft.Type())
		}
	default:
		return newError("Destino de asignación inválido: %T", leftNode)
	}
}

func applyFunction(fnObj object.Object, args []object.Object, callerEnv *object.Environment) object.Object {
	switch fn := fnObj.(type) {
	case *object.Function:
		extendedEnv := object.NewEnclosedEnvironment(fn.Env)
		if len(fn.Parameters) != len(args) {
			return newError("Función/Método '%s': número incorrecto de argumentos. Esperados %d, obtenidos %d",
				fn.Name, len(fn.Parameters), len(args))
		}
		for paramIdx, param := range fn.Parameters {
			extendedEnv.Set(param.Name.Value, args[paramIdx])
		}
		evaluated := Eval(fn.Body, extendedEnv)
		return unwrapReturnValue(evaluated)

	case *object.BoundMethod:
		extendedEnv := object.NewEnclosedEnvironment(fn.Env)
		if len(fn.Method.Parameters) != len(args) {
			return newError("Método '%s': número incorrecto de argumentos. Esperados %d, obtenidos %d",
				fn.Method.Name, len(fn.Method.Parameters), len(args))
		}
		for paramIdx, param := range fn.Method.Parameters {
			extendedEnv.Set(param.Name.Value, args[paramIdx])
		}
		evaluated := Eval(fn.Method.Body, extendedEnv)
		return unwrapReturnValue(evaluated)

	case *object.Builtin:
		return fn.Fn(callerEnv, args...)
	default:
		return newError("No es una función o método invocable: %s (valor: %s)", fnObj.Type(), fnObj.Inspect())
	}
}

// --- Funciones helper estándar (sin cambios) ---
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input { return TRUE	}
	return FALSE
}
func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!": return evalBangOperatorExpression(right)
	case "-": return evalMinusPrefixOperatorExpression(right)
	default: return newError("Operador prefijo desconocido: %s%s", operator, right.Type())
	}
}
func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE: return FALSE
	case FALSE: return TRUE
	case NULL: return TRUE
	default: return FALSE
	}
}
func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return newError("Operador menos unario desconocido: -%s", right.Type())
	}
	value := right.(*object.Integer).Value
	return &object.Integer{Value: -value}
}
func evalInfixExpression(operator string, left, right object.Object) object.Object {
	switch {
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)
	case left.Type() == object.STRING_OBJ && right.Type() == object.STRING_OBJ:
		return evalStringInfixExpression(operator, left, right)
	case operator == "==":
		return nativeBoolToBooleanObject(left == right)
	case operator == "!=":
		return nativeBoolToBooleanObject(left != right)
	case left.Type() != right.Type():
		return newError("Discrepancia de tipos: %s %s %s", left.Type(), operator, right.Type())
	default:
		return newError("Operador infijo desconocido: %s %s %s", left.Type(), operator, right.Type())
	}
}
func evalIntegerInfixExpression(operator string, left, right object.Object) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value
	switch operator {
	case "+": return &object.Integer{Value: leftVal + rightVal}
	case "-": return &object.Integer{Value: leftVal - rightVal}
	case "*": return &object.Integer{Value: leftVal * rightVal}
	case "/":
		if rightVal == 0 { return newError("División por cero.") }
		return &object.Integer{Value: leftVal / rightVal}
	case "<": return nativeBoolToBooleanObject(leftVal < rightVal)
	case ">": return nativeBoolToBooleanObject(leftVal > rightVal)
	case "==": return nativeBoolToBooleanObject(leftVal == rightVal)
	case "!=": return nativeBoolToBooleanObject(leftVal != rightVal)
	default: return newError("Operador entero desconocido: %s %s %s", left.Type(), operator, right.Type())
	}
}
func evalStringInfixExpression(operator string, left, right object.Object) object.Object {
	if operator != "+" {
		return newError("Operador de cadena desconocido: %s %s %s", left.Type(), operator, right.Type())
	}
	leftVal := left.(*object.String).Value
	rightVal := right.(*object.String).Value
	return &object.String{Value: leftVal + rightVal}
}
func evalIfExpression(ie *ast.IfExpression, env *object.Environment) object.Object {
	condition := Eval(ie.Condition, env)
	if isError(condition) { return condition }
	if isTruthy(condition) {
		return Eval(ie.Consequence, env)
	} else if ie.Alternative != nil {
		return Eval(ie.Alternative, env)
	} else {
		return NULL
	}
}
func evalExpressions(exps []ast.Expression, env *object.Environment) []object.Object {
	var result []object.Object
	for _, e := range exps {
		evaluated := Eval(e, env)
		if isError(evaluated) { return []object.Object{evaluated} }
		result = append(result, evaluated)
	}
	return result
}
func extendFunctionEnv(fn *object.Function, args []object.Object) *object.Environment {
	env := object.NewEnclosedEnvironment(fn.Env)
	for paramIdx, param := range fn.Parameters {
		if paramIdx < len(args) {
			env.Set(param.Name.Value, args[paramIdx])
		} else {
		}
	}
	return env
}
func unwrapReturnValue(obj object.Object) object.Object {
	if returnValue, ok := obj.(*object.ReturnValue); ok { return returnValue.Value }
	return obj
}
func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}
func isError(obj object.Object) bool {
	if obj != nil { return obj.Type() == object.ERROR_OBJ }
	return false
}
func isTruthy(obj object.Object) bool {
	switch obj {
	case NULL: return false
	case TRUE: return true
	case FALSE: return false
	default:
		return true
	}
}
func evalIndexExpression(left, index object.Object) object.Object {
	switch {
	case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
		return evalArrayIndexExpression(left, index)
	default:
		return newError("Operador de índice no soportado: %s", left.Type())
	}
}
func evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Array)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)
	if idx < 0 || idx > max { return NULL	}
	return arrayObject.Elements[idx]
}

var builtins = map[string]*object.Builtin{
	"len": &object.Builtin{
		Fn: func(env *object.Environment, args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("len() espera 1 argumento, se obtuvieron %d", len(args))
			}
			switch arg := args[0].(type) {
			case *object.String:
				return &object.Integer{Value: int64(len(arg.Value))}
			case *object.Array:
				return &object.Integer{Value: int64(len(arg.Elements))}
			default:
				return newError("Argumento para `len` no soportado, se obtuvo %s", args[0].Type())
			}
		},
	},
}
```

Con esto, el evaluador ahora tiene la lógica fundamental para manejar clases y miembros estáticos. Los modificadores de acceso siguen siendo un TODO.
