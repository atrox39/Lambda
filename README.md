# Lambda: A Minimal Programming Language

Lambda es un lenguaje de programación minimalista implementado en Go, con una sintaxis inspirada en TypeScript. Está diseñado para ser ligero y fácil de entender, sirviendo como un proyecto fundamental para explorar el diseño y la interpretación de lenguajes.

## Características

*   **Análisis Léxico:** Convierte el código fuente en tokens.
*   **Análisis Sintáctico (Parsing):** Construye un Árbol de Sintaxis Abstracta (AST) a partir de los tokens.
*   **Evaluación:** Interpreta el AST para producir resultados.
*   **Sintaxis similar a TypeScript:** Sintaxis familiar e intuitiva para los desarrolladores.

## Cómo Empezar

Para empezar con Lambda, necesitarás tener Go instalado en tu sistema.

1.  **Clonar el repositorio:**
    ```bash
    git clone https://github.com/atrox39/Lambda.git # Reemplaza con la URL real de tu repositorio
    cd Lambda
    ```

2.  **Ejecutar el intérprete:**
    Puedes ejecutar el archivo `main.go` directamente. Este proyecto probablemente proporciona un REPL (Read-Eval-Print Loop) o espera un archivo como entrada.

    ```bash
    go run main.go
    ```
    *(Las instrucciones adicionales sobre cómo usar el intérprete dependerán de su implementación, por ejemplo, `go run main.go <tu_script.ld>` o simplemente `go run main.go` para un REPL)*

## Ejemplo

Aquí tienes un ejemplo sencillo de código Lambda:

```lambda
let add = fn(a, b) {
    return a + b;
};

let result = add(5, 10);
result; // Esto debería producir 15
```

## Estructura del Proyecto

El proyecto está organizado en varios directorios clave:

*   `ast/`: Define los nodos del Árbol de Sintaxis Abstracta (AST).
*   `evaluator/`: Contiene la lógica para evaluar el AST.
*   `lexer/`: Implementa el analizador léxico (tokenizador).
*   `object/`: Define los diversos tipos de objetos utilizados en el lenguaje (por ejemplo, enteros, booleanos, funciones).
*   `parser/`: Implementa el analizador sintáctico, que transforma los tokens en un AST.
*   `token/`: Define los tipos y estructuras de los tokens.
*   `main.go`: El punto de entrada del intérprete.

## Contribuciones

¡Las contribuciones son bienvenidas! No dudes en abrir issues o enviar pull requests.

## Licencia

Este proyecto está bajo la Licencia MIT - consulta el archivo LICENSE para más detalles. *(Asumiendo MIT, o el usuario puede especificar)*
