GHC=ghc
LLVM_AS=llvm-as
LLI=lli
SRC=compiler.hs AbstractSyntax.hs CodeGen.hs Parser.hs StaticAnalysis.hs
compiler: $(SRC)
	$(GHC) --make compiler.hs
clean:
	rm -f *.hi *.o compiler baa.*
test:
	@for i in tests/*.t ; do echo $$i ; stack exec dragon-exe $$i > baa.ll ; $(LLVM_AS) baa.ll -f ; $(LLI) baa.bc ; done
