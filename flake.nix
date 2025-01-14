{
  description = "shell env";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs, ... }: 
  let
    system = flake-utils.lib.system.x86_64-linux;
    pkgs = nixpkgs.legacyPackages.${system};
    python = pkgs.python311.withPackages (p: [
      p.pandas
      p.jupyter
      p.matplotlib
      p.jupyterlab
      p.scipy
      p.scikitlearn
      p.numpy
      p.psycopg2
      p.requests
    ]);
  in
  {
    devShells.${system}.default = pkgs.mkShell {
        name = "env";
        buildInputs = [
          pkgs.jdk21
          pkgs.sbt
        ];
        runScript = "zsh";
    };
  };
}
