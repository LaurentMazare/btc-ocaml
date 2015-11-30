let
 pkgs = import ~/nixpkgs {};
 ocaml = pkgs.ocamlPackages_latest;
in rec {

  corebuild = pkgs.stdenv.mkDerivation (rec {
    name = "ocaml-corebuild";
    src = ocaml.core.src;
    buildCommand = ''
      tar xf "$src"
      mkdir -p "$out/bin"
      cp */corebuild "$out/bin"
    '';
    });


  btc_ocaml = pkgs.stdenv.mkDerivation (rec {
    name = "btc-ocaml";
    src = ./.;
    buildInputs = [
      corebuild ocaml.ocaml pkgs.zlib /* zlib should not be necessary, but it is */
      ] ++ (with ocaml; [
      findlib
      cryptokit core core_kernel async async_extra
      pa_ounit pa_structural_sexp sexplib comparelib fieldslib variantslib bin_prot]);
    buildCommand = ''
      cp "${src}"/* ./
      corebuild btc.native -pkgs core -pkgs async -pkgs cryptokit
      mkdir -p "$out/bin"
      cp btc.native "$out/bin/btc-ocaml"
    '';
  });
}
