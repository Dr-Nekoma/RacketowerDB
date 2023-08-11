# TODO: Check if you are already inside nix shell before running any just commands

build:
	raco exe --gui main.rkt

install:
	raco pkg install

remove NAME:
	raco pkg remove {{ NAME }}

uninstall NAME:
	raco pkg uninstall {{ NAME }}

docs NAME:
	raco docs {{ NAME }}

test:
	raco test
