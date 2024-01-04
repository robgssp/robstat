{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  };

  outputs = { self, nixpkgs }:
    let system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          (pkgs.sbcl.withPackages (pkgs: with pkgs; [
            local-time yason alexandria trivia cl-autowrap dbus swank
          ]))
        ];
        buildInputs = with pkgs; [ alsa-lib c2ffi clang ];

        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [ alsa-lib libfixposix libffi ]);
      };
    };
}
