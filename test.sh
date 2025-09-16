opam switch 5.1.0
eval $(opam env)
make
./spectec-core run-il nanorust_spec/*.spectec -p test.nrst