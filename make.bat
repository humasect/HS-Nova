ghc -iSource -hidir ./build -odir ./build -package OpenGL -main-is BuildContent --make BuildContent && ./BuildContent -DCALLCONV=stdcall
BuildContent.exe
