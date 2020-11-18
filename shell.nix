let
  ref = if builtins.currentSystem == "x86_64-darwin" then "nixpkgs-20.09-darwin" else "nixos-20.09";
  nixpkgs = builtins.fetchGit {
    inherit ref;
    url = "https://github.com/NixOS/nixpkgs.git";
  };
  pkgs = import nixpkgs {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      purescript
      spago
      nodejs-12_x
    ];
  }
