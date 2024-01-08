{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
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
            (pkgs.sbcl.withPackages (pkgs: with pkgs; [
              local-time yason alexandria trivia cl-autowrap dbus swank
            ]))
          ];
          buildInputs = with pkgs; [ alsa-lib c2ffi clang ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [ alsa-lib libfixposix libffi ]);
          shellHook = ''
            export CL_SOURCE_REGISTRY=`pwd`
          '';
        };
      });
}
