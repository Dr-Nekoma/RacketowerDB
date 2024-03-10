# TODO: Check if you are already inside nix shell before running any just commands

bplustree:
	gcc -c -fPIC utilities/bplustree.c -o utilities/bplustree.o
	gcc utilities/bplustree.o -shared -o libbplustree.so

build:
	raco exe --gui main.rkt

run: build bplustree
	./main

install:
	raco pkg install

remove NAME:
	raco pkg remove {{ NAME }}

uninstall NAME:
	raco pkg uninstall {{ NAME }}

docs NAME:
	raco docs {{ NAME }}

test:
	raco test main.rkt
