opam switch 5.1.0
eval $(opam env)
make exe
./spectec-core elab nanorust_spec/*.spectec > output.spectec
./spectec-core run-il nanorust_spec/*.spectec -p test.nrst -dbg > output.txt