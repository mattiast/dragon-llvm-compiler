GHC=ghc
LLVM_AS=/opt/llvm-2.1/bin/llvm-as
LLI=/opt/llvm-2.1/bin/lli
compiler:
	$(GHC) --make compiler.hs
clean:
	rm -f *.hi *.o compiler baa.*
test: compiler
	@for i in tests/*.t ; do echo $$i ; ./compiler $$i > baa.ll ; $(LLVM_AS) baa.ll -f ; $(LLI) baa.bc ; done
