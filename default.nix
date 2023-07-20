{ racket-minimal, lib, stdenv }:

stdenv.mkDerivation rec {
  name = "RacketowerDB";

  src = ./src;

  buildInputs = [ racket-minimal ];

  dontConfigure = true;

  buildPhase = ''
    racket 
    $CC -Wall -Wextra -std=c11 -pedantic -I${libX11}/include nwm.c -L${libX11}/lib -lX11 -o nwm
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv racketowerDB $out/bin/racketowerDB
  '';
}
