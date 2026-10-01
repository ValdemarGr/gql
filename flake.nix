{
  description = "shell env";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    codex-cli-nix.url = "github:sadjow/codex-cli-nix";
  };

  outputs = { flake-utils, self, nixpkgs, codex-cli-nix, ... }: 
  let
    system = flake-utils.lib.system.x86_64-linux;
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell {
        name = "env";
        buildInputs = [
          pkgs.jdk21
          pkgs.sbt
          codex-cli-nix.packages.${system}.default
        ];
        runScript = "zsh";
        shellHook = ''
          export SHELL='zsh'
        '';
    };
  };
}
