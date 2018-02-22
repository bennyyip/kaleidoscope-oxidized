.PHONY: all

all: fib
	./fib

fib.o: fib.ks
	cargo run -- -i fib.ks -o fib.o

fib: fib.o fib.c
	gcc fib.c fib.o -o fib
