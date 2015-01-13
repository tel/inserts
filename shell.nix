let
  pkgs            = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      inserts = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.inserts.name;
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
         hs.hoogle
         hs.pointfree
         hs.ghcMod
         hs.hlint
         hs.shake
         hs.stylishHaskell
       ] ++ hs.inserts.propagatedNativeBuildInputs)))
     ];
   }
