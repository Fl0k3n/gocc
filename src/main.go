package main

import (
	"asm"
	"codegen"
	"elf"
	"fmt"
	"grammars"
	"irs"
	"linkers"
	"parsers"
	"regexp"
	"semantics"
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

func testAll() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar")
	if err != nil {
		fmt.Println(err)
		return
	}
	grammar, err := grammarReader.Read()
	grammarReader.Finish()
	if err != nil {
		fmt.Println(err)
		return
	}
	tokenizer, err := tokenizers.New("../resources/csrc/functions.c", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	// p := parsers.NewForGrammar(grammar, tokenizer)
	p := parsers.NewFromFile(grammar, tokenizer, "/home/flok3n/misc/acttab.gob", "/home/flok3n/misc/gototab.gob", false)
	translationUnit, err := p.BuildParseTree()
	tokenizer.Finish()
	if err != nil {
		fmt.Println("Parser error")
		fmt.Println(err)
		return
	}
	et := semantics.NewErrorTracker()
	analyzer := semantics.NewAnalyzer(et)
	analyzer.Analyze(&translationUnit)
	if et.HasError() {
		et.PrintErrors()
	} else {
		irWriter := irs.NewWriter()
		irGen := irs.NewGenerator(irWriter)
		functionsIr, globals, typeEngine := irGen.Generate(&translationUnit)
		memoryManager := codegen.NewMemoryManager(typeEngine)
		registerAllocator := codegen.NewBasicAllocator(memoryManager)
		asmWriter := codegen.NewWriter()
		codeGen := codegen.NewGenerator(
			functionsIr, globals, registerAllocator, memoryManager, asmWriter, typeEngine,
		)
		assemblyCode := codeGen.Generate()
		relocator := asm.NewRelocator()
		assembler := asm.NewAssembler(relocator)
		assembledFunctions, assembledCode := assembler.Assemble(assemblyCode)
		assembler.PrintAssemblyAlongAssembledBytes()
		displacementsToFix := relocator.PrepareForRelocation(assembledCode)
		elfBuilder := elf.NewBuilder(assembledCode, assembledFunctions, globals, displacementsToFix)
		if err := elfBuilder.CreateRelocatableELF("test.c", "/home/flok3n/develop/from_scratch/gocc/resources/csrc/elf"); err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("OK")
		}
	}
}

func testAssembler() {
	writer := codegen.NewWriter()
	writer.EnterFunction(&irs.GlobalSymbol{Symbol: &irs.Symbol{Name: "test"}})
	raxFam := codegen.GetIntegralRegisterFamily(codegen.RAX)
	r10Fam := codegen.GetIntegralRegisterFamily(codegen.R10)
	rspFam := codegen.GetIntegralRegisterFamily(codegen.RSP)
	rcxFam := codegen.GetIntegralRegisterFamily(codegen.RCX)
	reg1 := codegen.GetIntegralRegisterFamily(codegen.R12).UseForSize(codegen.DWORD_SIZE)
	reg2 := codegen.GetIntegralRegisterFamily(codegen.RAX).UseForSize(codegen.QWORD_SIZE)
	// writer.MovIntegralRegisterToIntegralRegister(reg1, reg2) // mov eax, ecx -> 89 c8 
	// writer.MovMemoryToIntegralRegister(reg1, codegen.RegisterMemoryAccessor{reg2}) // mov eax, [ecx] -> 67 8b 01
	// writer.MovIntegralRegisterToMemory(codegen.RegisterMemoryAccessor{reg2}, reg1) // mov [ecx], eax -> 67 89 01
	// writer.MovIntegralRegisterToMemory(codegen.RegisterMemoryAccessor{reg1}, reg2) // mov [ecx], eax -> 67 89 01
	// writer.MovIntegralConstantToIntegralRegister(raxFam.UseForSize(codegen.QWORD_SIZE), 0x1234)
	// writer.MovIntegralConstantToIntegralRegister(raxFam.UseForSize(codegen.DWORD_SIZE), 0x1234)
	// writer.MovIntegralConstantToIntegralRegister(raxFam.UseForSize(codegen.WORD_SIZE), 0x1234)
	// writer.MovIntegralConstantToIntegralRegister(raxFam.UseForSize(codegen.BYTE_SIZE), 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg1}, 1, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg1}, 2, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg1}, 4, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg1}, 8, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg2}, 1, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg2}, 2, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg2}, 4, 0x12)
	// writer.MovIntegralConstantToMemory(codegen.RegisterMemoryAccessor{reg2}, 8, 0x12)
	// writer.MovIntegralRegisterToMemory(codegen.LabeledMemoryAccessor{"test"}, raxFam.UseForSize(codegen.QWORD_SIZE))
	// writer.MovIntegralRegisterToMemory(codegen.LabeledMemoryAccessor{"test"}, raxFam.UseForSize(codegen.DWORD_SIZE))
	// writer.MovMemoryToIntegralRegister(raxFam.UseForSize(codegen.QWORD_SIZE), codegen.LabeledMemoryAccessor{"test"})
	// writer.MovMemoryToIntegralRegister(raxFam.UseForSize(codegen.DWORD_SIZE), codegen.LabeledMemoryAccessor{"test"})
	// writer.JumpToLabel("test")
	// writer.JumpIfZero("test")
	// writer.PushIntegralReg(raxFam.UseForSize(codegen.QWORD_SIZE))
	// writer.PushIntegralReg(raxFam.UseForSize(codegen.WORD_SIZE))
	// writer.PushIntegralReg(rspFam.UseForSize(codegen.QWORD_SIZE))
	// writer.AddIntegralRegisters(raxFam.UseForSize(codegen.QWORD_SIZE), rcxFam.UseForSize(codegen.QWORD_SIZE))
	// writer.AddIntegralRegisters(raxFam.UseForSize(codegen.DWORD_SIZE), rcxFam.UseForSize(codegen.DWORD_SIZE))
	// writer.AddIntegralRegisters(raxFam.UseForSize(codegen.WORD_SIZE), rcxFam.UseForSize(codegen.WORD_SIZE))
	// writer.AddIntegralRegisters(raxFam.UseForSize(codegen.BYTE_SIZE), rcxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.AddIntegralRegisters(r10Fam.UseForSize(codegen.QWORD_SIZE), rcxFam.UseForSize(codegen.QWORD_SIZE))
	// writer.AddIntegralRegisters(r10Fam.UseForSize(codegen.DWORD_SIZE), rcxFam.UseForSize(codegen.DWORD_SIZE))
	// writer.AddIntegralRegisters(r10Fam.UseForSize(codegen.WORD_SIZE), rcxFam.UseForSize(codegen.WORD_SIZE))
	// writer.AddIntegralRegisters(r10Fam.UseForSize(codegen.BYTE_SIZE), rcxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.AddConstantInteger(raxFam.UseForSize(codegen.QWORD_SIZE), 1)
	// writer.AddConstantInteger(raxFam.UseForSize(codegen.DWORD_SIZE), 1)
	// writer.AddConstantInteger(raxFam.UseForSize(codegen.WORD_SIZE), 1)
	// writer.AddConstantInteger(raxFam.UseForSize(codegen.BYTE_SIZE), 1)
	// writer.AddConstantInteger(r10Fam.UseForSize(codegen.QWORD_SIZE), 1)
	// writer.AddConstantInteger(r10Fam.UseForSize(codegen.DWORD_SIZE), 1)
	// writer.AddConstantInteger(r10Fam.UseForSize(codegen.WORD_SIZE), 1)
	// writer.AddConstantInteger(r10Fam.UseForSize(codegen.BYTE_SIZE), 1)
	// writer.SignedMultiplyIntegralRegisters(raxFam.UseForSize(codegen.WORD_SIZE), r10Fam.UseForSize(codegen.WORD_SIZE))
	// writer.SignedMultiplyIntegralRegisters(raxFam.UseForSize(codegen.DWORD_SIZE), r10Fam.UseForSize(codegen.DWORD_SIZE))
	// writer.SignedMultiplyIntegralRegisters(raxFam.UseForSize(codegen.QWORD_SIZE), r10Fam.UseForSize(codegen.QWORD_SIZE))
	// writer.SignedDivideRaxRdxByIntegralRegister(rcxFam.UseForSize(codegen.QWORD_SIZE))
	// writer.SignedDivideRaxRdxByIntegralRegister(rcxFam.UseForSize(codegen.DWORD_SIZE))
	// writer.SignedDivideRaxRdxByIntegralRegister(rcxFam.UseForSize(codegen.WORD_SIZE))
	// writer.SignedDivideRaxRdxByIntegralRegister(rcxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(raxFam.UseForSize(codegen.QWORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(raxFam.UseForSize(codegen.DWORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(raxFam.UseForSize(codegen.WORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(r10Fam.UseForSize(codegen.QWORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(r10Fam.UseForSize(codegen.DWORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(r10Fam.UseForSize(codegen.WORD_SIZE), raxFam.UseForSize(codegen.BYTE_SIZE))
	// writer.ZeroExtend(r10Fam.UseForSize(codegen.DWORD_SIZE), raxFam.UseForSize(codegen.WORD_SIZE))
	writer.Reference(raxFam.UseForSize(codegen.QWORD_SIZE), codegen.RegisterMemoryAccessor{Register: raxFam.UseForSize(codegen.QWORD_SIZE)})
	writer.Reference(raxFam.UseForSize(codegen.QWORD_SIZE), codegen.StackFrameOffsetMemoryAccessor{Offset: 4})
	writer.Reference(raxFam.UseForSize(codegen.QWORD_SIZE), codegen.GOTMemoryAccessor{Symbol: nil})
	fmt.Println(raxFam, r10Fam, rspFam, reg1, reg2, rcxFam)
	assembly := writer.GetAssembly()
	asmLines := []codegen.AsmLine{}
	for _, f := range assembly {
		asmLines = append(asmLines, f.Code...)
	}
	relocator := asm.NewRelocator()
	assembler := asm.NewAssembler(relocator)
	assembler.AssembleMultiple(asmLines)
	assembler.PrintAssemblyAlongAssembledBytes()
	fmt.Println("")
}

func serializeTables() {
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
	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.SerializeTables("/home/flok3n/misc/acttab.gob", "/home/flok3n/misc/gototab.gob")
}

func testLinker() {
	linker := linkers.New()
	err := linker.CreateExecutable("/home/flok3n/develop/from_scratch/gocc/resources/csrc/elf",
							"/home/flok3n/develop/from_scratch/gocc/resources/csrc/linkerres")
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("ok")
	}
}

func main() {
	// testParserSimple2()
	// testAll()
	// testGrammarReader()
	// testTokenizer()
	// testTableBuilder()
	// testTableBuilder2()
	// testAssembler()
	// serializeTables()
	testLinker()
}
