{ mkDerivation, base, either, mtl, stdenv }:
mkDerivation {
  pname = "hoist-error";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base either mtl ];
  description = "Some convenience facilities for hoisting errors into a monad";
  license = stdenv.lib.licenses.mit;
}
