{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          robstat = pkgs.callPackage ./default.nix { };
          default = self.packages.${system}.robstat;
        };

        devShells.default = pkgs.mkShell {
          packages = [
            (pkgs.sbcl.withPackages (_: self.packages.${system}.robstat.lispLibs))
          ];
          buildInputs = with pkgs; [ alsa-lib c2ffi clang ];
          inputsFrom = [ self.packages.${system}.default ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [ alsa-lib libfixposix libffi ]);
          shellHook = ''
            export CL_SOURCE_REGISTRY=`pwd`
            export LIBC=${pkgs.stdenv.cc.libc.dev}
        # #   '';
        };
      });
}
