{ sources ? import ./nix/sources.nix
# , nixpkgs ? import sources.nixpkgs {}
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc902"
}:

let
  # inherit (nixpkgs) pkgs;
  
  # since we are in a sub-directory
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "hsamp" =
        hself.callCabal2nix "hsamp" (gitignore ./.) {};

      # formatting =
      #   hself.callHackage "formatting" "7.1.2" {};
      # formatting = hself.formatting_7_1_2;

      # looks like readable needs some help circa Nov 2022
      # but how can we get nix to pick up the revised version
      # and not the old 0.3.1 version... Looks like need to
      # update all-cabal-hashes, but where is it?
      #
      #readable =
      #   hself.callHackage "readable" "0.3.1" {};
      
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."hsamp"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      # pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.git
    ];
    # withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."hsamp");

in
  {
   inherit shell;
   inherit exe;
   inherit myHaskellPackages;
   "hsamp" = myHaskellPackages."hsamp";
  }

