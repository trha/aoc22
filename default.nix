let hls925 = with import <nixpkgs> {
  overlays = [
    (self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: {
          mkDerivation = args: hsuper.mkDerivation (args // {
            doCheck = false;
          });
        };
      };
    })
  ];
}; haskell.packages.ghc925.haskell-language-server;
in
with import <nixpkgs> {};
{
  shell = mkShell {
    nativeBuildInputs = [
      llvmPackages_12.clang llvmPackages_12.llvm 
      hls925 haskell.compiler.ghc925 cabal-install

    ];
  };
}
