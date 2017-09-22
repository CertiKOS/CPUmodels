let pkgs =import <nixpkgs> {}; 
in
with pkgs;

stdenv.mkDerivation {
  name = "cpumodels";

  buildInputs = with ocamlPackages_4_02; [
    ocaml findlib coq_8_5
    coqPackages_8_5.flocq
  ];

}
