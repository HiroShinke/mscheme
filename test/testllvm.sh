

stack exec mschemellvm testllvm.sc
llc testllvm.ll 
gcc -o testllvm testllvm.s
./testllvm
