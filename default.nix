{ pkgs, stdenv, lib, sbcl, wrapLisp, alsa-lib, pulseaudio, c2ffi, clang, makeWrapper }:
let
  sbclScript = wrapLisp {
    pkg = sbcl;
    faslExt = "fasl";
    flags = [ "--script" ];
  };
  pkg = sbclScript.buildASDFSystem {
    pname = "robstat";
    version = "0.1";
    src = ./.;
    lispLibs = with sbcl.pkgs; [ local-time yason alexandria
                                 trivia cl-autowrap cl-plus-c dbus swank
                                 cl-ppcre ];
    LIBC = stdenv.cc.libc.dev;
    nativeLibs = [ alsa-lib pulseaudio ];
    nativeBuildInputs = [ clang makeWrapper ];
    buildScript = pkgs.writeText "build-robstat.lisp" ''
      (load "${pkg.asdfFasl}/asdf.${pkg.faslExt}")
      (ensure-directories-exist #p"./spec/")
      (asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
      (asdf:load-system :robstat)
      ;; (sb-ext:save-lisp-and-die "robstat" :executable t :toplevel #'robstat:main )
      (asdf:operate 'asdf:program-op :robstat)
    '';
    installPhase = ''
      echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
      mkdir -pv $out/bin
      cp -rv ./* $out/
      cp -v ./robstat $out/bin
      wrapProgram $out/bin/robstat \
        --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
    '';
  };
in pkg
