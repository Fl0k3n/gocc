package main

import (
	"asm"
	"compilers"
	"errors"
	"flag"
	"fmt"
	"linkers"
	"playground"
	"strings"
)

const DEFAULT_ENTRY_SYMBOL = "main"
// TODO .conf
var DEFAULT_SHARED_OBJECT_DIRS = []string{
	"/home/flok3n/develop/from_scratch/gocc/resources/csrc/link/simple",
	"/home/flok3n/develop/from_scratch/gocc/resources/csrc/link",
	"/home/flok3n/develop/from_scratch/gocc/resources/csrc",
}

func test() {
	playground.Test()
}

type arrayFlags []string


func (i *arrayFlags) String() string {
    return strings.Join([]string(*i), ",")
}

func (i *arrayFlags) Set(value string) error {
    *i = append(*i, value)
    return nil
}

func cli() {
	var err error
	compile := flag.Bool("c", false, "Compile C source into relocatable file")
	linkReloc := flag.Bool("r", false, "Link relocatable files into relocatable file")
	linkExec := flag.Bool("e", false, "Create executable from relocatable file")
	linkShared := flag.Bool("s", false, "Create shared library from relocatable file")
	linkDynExec := flag.Bool("d", false, "Create dynamically linkable executable from relocatable file")
	showIr := flag.Bool("show-ir", false, "Print IR of code when compiling")
	showAsm := flag.Bool("show-asm", false, "Show generated assembly and bytecode when compiling")
	executableEntry := flag.String("entry", "", "Entry point of executable")
	soname := flag.String("soname", "", "Name of generated shared object")
	var sharedLibFlags arrayFlags
	flag.Var(&sharedLibFlags, "l", "Name of shared library to link against, can be used multiple times")
	var sharedLibDirectories arrayFlags
	flag.Var(&sharedLibDirectories, "L", "Path where linker should search for shared objects, can be used multiple times")
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
		compiler := compilers.New(compilers.Config{ParserVerbose: false, PrintIR: *showIr, PrintAssembly: *showAsm})
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
	} else if *linkShared || *linkDynExec {
		relocator := asm.NewRelocator()
		assembler := asm.NewAssembler(relocator)
		dynamicLinker := linkers.NewDynamicLinker(assembler)
		input := flag.Args()
		if len(input) == 0 {
			err = errors.New("Expected input Relocatable ELF file")
		}
		soDirs := []string(sharedLibDirectories)
		if len(soDirs) == 0 {
			soDirs = DEFAULT_SHARED_OBJECT_DIRS
		}
		if *linkShared {
			if *soname == "" {
				err = errors.New("Soname is required")
			}
			err = dynamicLinker.CreateSharedLibrary(input[0], *output, *soname, soDirs, []string(sharedLibFlags))
			if err != nil {
				goto onerr
			}
		} else {
			if *executableEntry == "" {
				fmt.Println("Warning: Entry point unspecified, defaulting to " + DEFAULT_ENTRY_SYMBOL)
				*executableEntry = DEFAULT_ENTRY_SYMBOL
			}
			err = dynamicLinker.CreateDynamicallyLinkedExecutable(input[0], *output, *executableEntry, soDirs, []string(sharedLibFlags))
			if err != nil {
				goto onerr
			}
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
