// main.go
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/atrox39/lambda/evaluator"
	"github.com/atrox39/lambda/lexer"
	"github.com/atrox39/lambda/object"
	"github.com/atrox39/lambda/parser"
)

const (
	lambdaVersion = "0.1.0"
	aboutMessage  = "Lambda is a simple, interpreted language based on TypeScript syntax, implemented in Go."
)

func main() {
	helpFlag := flag.Bool("help", false, "Show help message")
	aboutFlag := flag.Bool("about", false, "Show information about Lambda")
	versionFlag := flag.Bool("version", false, "Show Lambda version")
	filePathFlag := flag.String("file", "", "Path to the .ld file to interpret (optional if provided as a direct argument)")

	flag.Parse()

	// Handle flags first
	if *helpFlag {
		flag.Usage()
		return
	}

	if *aboutFlag {
		fmt.Println(aboutMessage)
		return
	}

	if *versionFlag {
		fmt.Printf("Lambda Version: %s\n", lambdaVersion)
		return
	}

	// Determine the file path
	var filePath string
	args := flag.Args()

	if *filePathFlag != "" {
		filePath = *filePathFlag
	} else if len(args) == 1 {
		filePath = args[0]
	} else if len(args) > 1 {
		fmt.Println("Error: Too many arguments. Please provide only one .ld file path or use the --file flag.")
		flag.Usage()
		os.Exit(1)
	} else {
		fmt.Println("Please provide a .ld file to interpret.")
		flag.Usage()
		os.Exit(1)
	}

	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		fmt.Printf("Error reading file %s: %s\n", filePath, err)
		os.Exit(1)
	}

	input := string(data)
	l := lexer.NewLexer(input)
	p := parser.NewParser(l)
	program := p.ParseProgram()

	if len(p.Errors()) != 0 {
		printParserErrors(p.Errors())
		os.Exit(1)
	}

	env := object.NewEnvironment()
	evaluated := evaluator.Eval(program, env)

	if evaluated != nil && evaluated.Type() == object.ERROR_OBJ {
		fmt.Printf("Runtime Error: %s\n", evaluated.Inspect())
	}
}

func printParserErrors(errors []string) {
	fmt.Println("Parser errors:")
	for _, msg := range errors {
		fmt.Printf("\t%s\n", msg)
	}
}
