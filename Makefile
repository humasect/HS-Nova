content:
	ghc -iSource -hidir ./build -odir ./build -package OpenGL -package OpenAL -main-is BuildContent --make BuildContent -DCALLCONV=ccall && ./BuildContent

clean:
	rm -rf ./Content/* BuildContent Web/Web
	make -C Source clean
