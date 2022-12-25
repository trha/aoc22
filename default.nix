with import <nixpkgs> {};
{
  shell = mkShell {
    nativeBuildInputs = [
      llvmPackages_12.clang llvmPackages_12.llvm pkg-config ncurses.dev zlib gmp libiconv libffi
    ];
    shellHooks = ''
      export PATH=$PATH:~/.cabal/bin:~/.ghcup/bin
    '';
  };
}
