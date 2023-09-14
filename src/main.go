package main

import (
	"compilers"
	"errors"
	"flag"
	"fmt"
	"linkers"
	"playground"
)

const DEFAULT_ENTRY_SYMBOL = "main"

func test() {
	playground.Test()
}

func cli() {
	var err error
	compile := flag.Bool("c", false, "Compile C source into relocatable file")
	linkReloc := flag.Bool("r", false, "Link relocatable files into relocatable file")
	linkExec := flag.Bool("e", false, "Create executable from relocatable file")
	executableEntry := flag.String("entry", "", "Entry point of executable")
	output := flag.String("o", "", "Output file name")
	flag.Parse()

	if len(flag.Args()) > 0 && *output == "" {
		err = errors.New("Output file path is required, use -o=path")
		goto onerr
	}
	if *compile {
		input := flag.Args()
		if len(input) == 0 {
			err = errors.New("Expected input C file")
			goto onerr
		}
		compiler := compilers.New(compilers.Config{ParserVerbose: false, PrintIR: true, PrintAssembly: true})
		err = compiler.Compile(input[0], *output)
		if err != nil {
			goto onerr
		}
	} else if *linkExec {
		linker := linkers.New()
		input := flag.Args()
		if len(input) == 0 {
			err = errors.New("Expected input Relocatable ELF file")
			goto onerr
		}
		if *executableEntry == "" {
			fmt.Println("Warning: Entry point unspecified, defaulting to " + DEFAULT_ENTRY_SYMBOL)
			*executableEntry = DEFAULT_ENTRY_SYMBOL
		}
		if er := linker.CreateExecutable(input[0], *output, *executableEntry); er != nil {
			err = er
			goto onerr
		}
	} else if *linkReloc {
		linker := linkers.New()
		input := flag.Args()
		if len(input) == 0 {
			err = errors.New("Expected list of Relocatable ELF files to link")
			goto onerr
		}
		if er := linker.StaticLinkRelocatablesIntoRelocatable(input, *output); er != nil {
			err = er	
			goto onerr
		}
	} else {
		// err = errors.New("Unspecified mode, aborting.")
		test()
	}
onerr:
	if err != nil {
		fmt.Println("Error: ")
		fmt.Println(err)
	} else {
		fmt.Println("OK")
	}
}

func main() {
	cli()
}
