{ rpRef ? "ed34cbf98eebd42a2025913cbeec77e839f172f7", rpSha ?  "130lv6wh7j9m0qncpjf7693lxcp1dy4y73wkf0mqp4bnndqyi9kn" }:

let rp = (import <nixpkgs> {}).fetchFromGitHub {
           owner = "reflex-frp";
           repo = "reflex-platform";
           rev = rpRef;
           sha256 = rpSha;
         };

in
  (import rp {}).project ({ pkgs, ... }: {
    name = "pact-web-umbrella";
    overrides = self: super: {
      pact = self.callPackage (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "pact";
        rev = "a85363e915af37d06bec777814b4fcdba90779df";
        sha256 = "07n6hb41lpj99r33712kqyvj610wk3n7n1404mmnh8g7l3wvsgjc";
      }) {};
    };
    packages = {
      pact-web-repl = ./.;
    };
    
    shells = {
      ghc = ["pact-web-repl"];
      ghcjs = ["pact-web-repl"];
    };
  
  })
