RTS = +RTS -N2 -RTS
GHC = ghc $(RTS) -hidir ../build/ -odir ../build/
#-optl-s
HSCS = Nova/GL.hs Nova/AL.hs

#-optc-O3 -optc-march=pentium4 -optc-mfpmath=sse
#-fexcess-precision
#-no-recomp
#-fspec-constr

all:
	@echo "'make exec' or 'make shell'"

exec: $(HSCS)
	$(GHC) -threaded --make Main -DCALLCONV=ccall

execprof: $(HSCS)
	$(GHC) -threaded -prof -auto-all --make Main -o Main.p -DCALLCONV=ccall

shell: $(HSCS)
	$(GHC) -threaded -package ghc -main-is Shell --make Shell -DCALLCONV=ccall

shellprof: Nova/GL.hs Nova/AL.hs
	$(GHC) -threaded -prof -auto-all -package ghc -main-is Shell --make Shell -l Shell.p -DCALLCONV=ccall

dump-simpl:
	$(GHC) -i../build -ddump-simpl -c EditUI.hs > simpl

docs: $(HSCS)
#	for i in $(HSCS); do \
#		find . -name "$$i" | xargs sed -i '' -e 's/CALLCONV/ccall/g'; \
#	done
	haddock -d --optghc=-threaded --optghc=-hidir --optghc=../build/ --optghc=-odir --optghc=../build/ \
		--optghc=-DCALLCONV=ccall --optghc=-package --optghc=ghc \
		--ignore-all-exports -t Novapilot -h -o ../build \
		`for i in */*.hs; do echo $$i; done` \
		`for i in *.hs; do echo $$i; done`

clean:
	rm -rf *.{hi,o} ../build/* Shell.p Shell Main.p Main $(HSCS)

Nova/GL.hs: Nova/GL.hsc
	hsc2hs Nova/GL.hsc

Nova/AL.hs: Nova/AL.hsc
	hsc2hs Nova/AL.hsc
