package compilers

import (
	"asm"
	"ast"
	"codegen"
	"elf"
	"errors"
	"grammars"
	"irs"
	"os"
	"parsers"
	"path/filepath"
	"semantics"
	"tokenizers"
)

// TODO .env
const GRAMMAR_FILE_NAME = "ansi_c_grammar"
const ACTION_TABLE_NAME = ".acctab.gob"
const GOTO_TABLE_NAME = ".gototab.gob"
const RESOURCES = "resources"

type Config struct {
	ParserVerbose bool
	PrintIR bool
	PrintAssembly bool
}

func DefaultConfig() Config {
	return Config{
		ParserVerbose: false,
		PrintIR: false,
		PrintAssembly: false,
	}
}

type Compiler struct {
	conf Config	
	actionTablePath string
	gotoTablePath string
	grammarPath string
}

func New(conf Config) *Compiler {
	exec, err := os.Executable()
	if err != nil {
		panic(err)
	}
	resourcesPath := filepath.Join(filepath.Dir(exec), "..", RESOURCES)

	return &Compiler{
		conf: conf,
		grammarPath: filepath.Join(resourcesPath, GRAMMAR_FILE_NAME),
		actionTablePath: filepath.Join(resourcesPath, ACTION_TABLE_NAME),
		gotoTablePath: filepath.Join(resourcesPath, GOTO_TABLE_NAME),
	}
}

func (c *Compiler) SerializeParserTables() error {
	grammarReader, err := grammars.NewReader(c.grammarPath)
	if err != nil {
		return err
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	if err != nil {
		return err
	}
	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.SerializeTables(c.actionTablePath, c.gotoTablePath)
	return nil
}

func (c *Compiler) loadGrammar() (*grammars.Grammar, error) {
	grammarReader, err := grammars.NewReader(c.grammarPath)
	if err != nil {
		return nil, err
	}
	defer grammarReader.Finish()
	return grammarReader.Read()
}

func (c *Compiler) createTokenizer(inputPath string, grammar *grammars.Grammar) (*tokenizers.Tokenizer, error) {
	return tokenizers.New(inputPath, grammar)
}

func (c *Compiler) createParser(grammar *grammars.Grammar, tokenizer *tokenizers.Tokenizer) (*parsers.Parser, error) {
	_, err := parsers.NewFromFile(grammar, tokenizer, c.actionTablePath, c.gotoTablePath, c.conf.ParserVerbose)
	if err != nil {
		if err = c.SerializeParserTables(); err != nil {
			return nil, err
		}
	}
	return parsers.NewFromFile(grammar, tokenizer, c.actionTablePath, c.gotoTablePath, c.conf.ParserVerbose)
}

func (c *Compiler) parse(inputPath string) (ast.TranslationUnit, error) {
	grammar, err := c.loadGrammar()
	if err != nil {
		return ast.TranslationUnit{}, err
	}
	tokenizer, err := c.createTokenizer(inputPath, grammar)
	if err != nil {
		return ast.TranslationUnit{}, err
	}
	defer tokenizer.Finish()
	parser, err := c.createParser(grammar, tokenizer)
	if err != nil {
		return ast.TranslationUnit{}, err
	}
	return parser.BuildParseTree()
}

func (c *Compiler) performSemanticAnalysis(translationUnit *ast.TranslationUnit) (error) {
	et := semantics.NewErrorTracker()
	analyzer := semantics.NewAnalyzer(et)
	analyzer.Analyze(translationUnit)
	if et.HasError() {
		et.PrintErrors()
		return errors.New("Found semantic or type errors, compilation aborted.")
	}
	return nil
}

func (c *Compiler) generateIntermediateRepresentation(translationUnit *ast.TranslationUnit) *irs.IntermediateRepresentation {
	irWriter := irs.NewWriter()
	irGen := irs.NewGenerator(irWriter)
	ir := irGen.Generate(translationUnit)
	if c.conf.PrintIR {
		irGen.PrintIR()
	}
	return ir
}

func (c *Compiler) generateCode(ir *irs.IntermediateRepresentation) *codegen.CodeRepresentation{
	memoryManager := codegen.NewMemoryManager(ir.BootstrappedTypeEngine)
	registerAllocator := codegen.NewBasicAllocator(memoryManager)
	asmWriter := codegen.NewWriter()
	codeGen := codegen.NewGenerator(ir, registerAllocator, memoryManager, asmWriter)
	return codeGen.Generate()
}

func (c *Compiler) assemble(assemblyCode []*codegen.FunctionCode) ([]*asm.AssembledFunction, []uint8, []asm.DisplacementToFix){
	relocator := asm.NewRelocator()
	assembler := asm.NewAssembler(relocator)
	assembledFunctions, assembledCode := assembler.Assemble(assemblyCode)
	if c.conf.PrintAssembly {
		assembler.PrintAssemblyAlongAssembledBytes()
	}
	displacementsToFix := relocator.PrepareForRelocation(assembledCode)
	return assembledFunctions, assembledCode, displacementsToFix
}

func (c *Compiler) writeRelocatableFile(
	codeRepr *codegen.CodeRepresentation,
	assembledFunctions []*asm.AssembledFunction,
	assembledCode []uint8,
	displacementsToFix []asm.DisplacementToFix,
	inputPath string,
	outputPath string,
) error {
	inputFileName := filepath.Base(inputPath)
	elfBuilder := elf.NewBuilder(assembledCode, assembledFunctions, codeRepr.Globals, codeRepr.Rodata, displacementsToFix)
	return elfBuilder.CreateRelocatableELF(inputFileName, outputPath)
}

func (c *Compiler) Compile(inputPath string, outputPath string) error {
	translationUnit, err := c.parse(inputPath)
	if err != nil {
		return err
	}

	err = c.performSemanticAnalysis(&translationUnit)
	if err != nil {
		return err
	}

	ir := c.generateIntermediateRepresentation(&translationUnit)

	codeRepr := c.generateCode(ir)

	assembledFunctions, assembledCode, displacementsToFix := c.assemble(codeRepr.Code)

	return c.writeRelocatableFile(codeRepr, assembledFunctions, assembledCode, displacementsToFix, inputPath, outputPath)
}
