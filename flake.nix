{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Send and receive messages using the Simple Application Message Protocol (SAMP)";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      # supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        hsamp = final.haskellPackages.callCabal2nix "hsamp" ./. {};
      });
      packages = forAllSystems (system: {
         hsamp = nixpkgsFor.${system}.hsamp;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.hsamp);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.hsamp];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            # haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
