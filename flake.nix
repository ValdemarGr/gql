{
  description = "Shell for dev";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs, ... }: 
  let
    system = flake-utils.lib.system.x86_64-linux;
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell {
      name = "catcheffect-dev";
      nativeBuildInputs = [ 
        pkgs.jdk11
        pkgs.scalafmt
        pkgs.zsh
        pkgs.sbt
        pkgs.graalvm-ce
      ];
    };
  };
}
