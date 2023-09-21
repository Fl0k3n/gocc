package playground

import (
	"asm"
	"compilers"
	"fmt"
	"grammars"
	"linkers"
	"parsers"
	"path/filepath"
	"regexp"
	"tokenizers"
)

func testGrammarReader() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(grammar)
}

func testTokenizer() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	// tokenizer, err := tokenizers.New("../resources/csrc/simple.c", grammar)
	tokenizer, err := tokenizers.New("../resources/csrc/functions.c", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	tokens := make([]tokenizers.Token, 0)
	for {
		tokenizer.Advance()
		token := tokenizer.LastToken()
		if token.T == tokenizers.EOF {
			break
		}
		tokens = append(tokens, token)
	}
	fmt.Println(tokens)
}

func testTableBuilder() {
	grammar := &grammars.Grammar{
		Terminals: []string{"a", "b"},
		Nonterminals: []string{"S'", "S", "X"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("X"), grammars.Nonterminal("X")}, ProdId: 1},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("a"), grammars.Nonterminal("X")}, ProdId: 2},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("b")}, ProdId: 3},
		},
		StringsToTokenTypes: map[string]string{"a": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	// grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar")
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }
	// defer grammarReader.Finish()
	// grammar, err := grammarReader.Read()
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }

	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.PrintConfigurations()
}

func testTableBuilder2() {
	grammar := &grammars.Grammar{
		Terminals: []string{"=", "+", "int", "id", "(", ")"},
		Nonterminals: []string{"S'", "S", "V", "E", "F"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("V"), grammars.Terminal("="), grammars.Nonterminal("E")}, ProdId: 1},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("F")}, ProdId: 2},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("E"), grammars.Terminal("+"), grammars.Nonterminal("F")}, ProdId: 3},
			{From: "F", To: []grammars.Symbol{grammars.Nonterminal("V")}, ProdId: 4},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("int")}, ProdId: 5},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("("), grammars.Nonterminal("E"), grammars.Terminal(")")}, ProdId: 6},
			{From: "V", To: []grammars.Symbol{grammars.Terminal("id")}, ProdId: 7},
		},
		StringsToTokenTypes: map[string]string{"=": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	// grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar")
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }
	// defer grammarReader.Finish()
	// grammar, err := grammarReader.Read()
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }

	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.PrintConfigurations()
}

func testParserSimple() {
	grammar := &grammars.Grammar{
		Terminals: []string{"a", "b"},
		Nonterminals: []string{"S'", "S", "X"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("X"), grammars.Nonterminal("X")}, ProdId: 1},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("a"), grammars.Nonterminal("X")}, ProdId: 2},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("b")}, ProdId: 3},
		},
		StringsToTokenTypes: map[string]string{"a": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	tokenizer, err := tokenizers.New("../resources/sample", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	p := parsers.NewForGrammar(grammar, tokenizer, true)
	p.BuildParseTree()
}

func testParserSimple2() {
	grammar := &grammars.Grammar{
		Terminals: []string{"=", "+", "int", "id", "(", ")"},
		Nonterminals: []string{"S'", "S", "V", "E", "F"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("V"), grammars.Terminal("="), grammars.Nonterminal("E")}, ProdId: 1},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("F")}, ProdId: 2},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("E"), grammars.Terminal("+"), grammars.Nonterminal("F")}, ProdId: 3},
			{From: "F", To: []grammars.Symbol{grammars.Nonterminal("V")}, ProdId: 4},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("int")}, ProdId: 5},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("("), grammars.Nonterminal("E"), grammars.Terminal(")")}, ProdId: 6},
			{From: "V", To: []grammars.Symbol{grammars.Terminal("id")}, ProdId: 7},
		},
		StringsToTokenTypes: map[string]string{"id": "id", "(": "(", ")": ")", "int": "int", "+": "+", "=": "="},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	tokenizer, err := tokenizers.New("../resources/sampleexpr", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	p := parsers.NewForGrammar(grammar, tokenizer, true)
	p.BuildParseTree()
}

func testCompiler() {
	
}

func testLinker() {
	linker := linkers.New()
	err := linker.CreateExecutable("/home/flok3n/develop/from_scratch/gocc/resources/csrc/elf",
							"/home/flok3n/develop/from_scratch/gocc/resources/csrc/linkerres",
							"main")
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("ok")
	}
}

func testStaticLinking() {
	linker := linkers.New()
	err := linker.StaticLinkRelocatablesIntoRelocatable(
		[]string{
			"/home/flok3n/develop/from_scratch/gocc/resources/csrc/elf1",
			"/home/flok3n/develop/from_scratch/gocc/resources/csrc/elf2",
		}, 
		"/home/flok3n/develop/from_scratch/gocc/resources/csrc/staticres",
	)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("ok")
	}
}

func testSharedLib() {
	relocator := asm.NewRelocator()
	assembler := asm.NewAssembler(relocator)
	dynamicLinker := linkers.NewDynamicLinker(assembler)
	err := dynamicLinker.CreateSharedLibrary(
		"/home/flok3n/develop/from_scratch/gocc/resources/csrc/link/simple/libmys2.o",
		"/home/flok3n/develop/from_scratch/gocc/resources/csrc/link/simple/libs2.so.0.0",
		"libs2.so.0", []string{})
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("ok")
	}
}

func CompileAndLink(srcs ...string) {
	exePath := "/home/flok3n/develop/from_scratch/gocc/resources/csrc/exe5"
	linkedPath := "/home/flok3n/develop/from_scratch/gocc/resources/csrc/linked5"
	compiledPrefix := "compiled"
	compiledPaths := []string{}

	for i, src := range srcs {
		dir := filepath.Dir(src)
		filename := filepath.Base(src)
		outputPath := filepath.Join(dir, fmt.Sprintf("%s%d", compiledPrefix, i))
		if err := compilers.New(compilers.DefaultConfig()).Compile(src, outputPath); err != nil {
			panic(err)
		}
		compiledPaths = append(compiledPaths, outputPath)
		fmt.Printf("Compiled %s to %s\n", filename, filepath.Base(outputPath))
	}

	linker := linkers.New()
	if err := linker.StaticLinkRelocatablesIntoRelocatable(compiledPaths, linkedPath); err != nil {
		panic(err)
	}
	fmt.Println("Linked to " + linkedPath)
	if err := linker.CreateExecutable(linkedPath, exePath, "main"); err != nil {
		panic(err)
	}
	fmt.Println("Built executable at " + exePath)
}

func Test() {
	// testParserSimple2()
	// testAll()
	// testLinker()
	// testGrammarReader()
	// testTokenizer()
	// testTableBuilder()
	// testTableBuilder2()
	// testAssembler()
	// serializeTables()
	// testStaticLinking()
	testSharedLib()
	// CompileAndLink(
	// 	"/home/flok3n/develop/from_scratch/gocc/resources/csrc/f1.c",
	// 	"/home/flok3n/develop/from_scratch/gocc/resources/csrc/f2.c",
	// )
	// CompileAndLink("/home/flok3n/develop/from_scratch/gocc/resources/csrc/f3.c")
}