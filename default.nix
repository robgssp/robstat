{ pkgs, lib, sbcl, alsa-lib, c2ffi, makeBinaryWrapper }:
let
  pkg = sbcl.buildASDFSystem {
    pname = "robstat";
    version = "0.1";
    src = ./.;
    lispLibs = with sbcl.pkgs; [ local-time yason alexandria
                                 trivia cl-autowrap dbus swank ];
    nativeLibs = [ alsa-lib ];
    nativeBuildInputs = [ c2ffi makeBinaryWrapper ];
    flags = [ "--script" ];
    buildScript = pkgs.writeText "build-robstat.lisp" ''
      (load "${pkg.asdfFasl}/asdf.${pkg.faslExt}")
      (ensure-directories-exist #p"./spec/")
      (asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
      (asdf:load-system :robstat)
      ;; (sb-ext:save-lisp-and-die "robstat" :executable t :toplevel #'robstat:main)
      (asdf:operate 'asdf:program-op :robstat)
    '';
    installPhase = ''
      echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
      mkdir -pv $out/bin
      cp -v ./robstat $out/bin
      wrapProgram $out/bin/robstat \
        --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
    '';
  };
in pkg
