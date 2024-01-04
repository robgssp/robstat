{ pkgs, lib, sbcl, alsa-lib, c2ffi }:
let alexandria = sbcl.pkgs.alexandria;
in sbcl.buildASDFSystem {
  pname = "robstat";
  version = "0.1";
  src = ./.;
  lispLibs = with sbcl.pkgs; [ local-time yason alexandria
                               trivia cl-autowrap dbus swank ];
  nativeBuildInputs = [ alsa-lib c2ffi ];
  buildScript = pkgs.writeText "build-robstat.lisp" ''
    ;; (load "${alexandria.asdfFasl}/asdf.${alexandria.faslExt}")
    (require :asdf)
    (ensure-directories-exist #p"./spec/")
    (asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
    (asdf:load-system :robstat)
    (sb-ext:save-lisp-and-die "robstat" :executable t :toplevel #'robstat:main)
    ;; (asdf:operate 'asdf:program-op :robstat)
  '';
  LD_LIBRARY_PATH = lib.makeLibraryPath [ pkgs.alsa-lib ];
  # installPhase = ''
  #   mkdir -pv $out/bin
  #   cp -v ./robstat $out/bin
  #   wrapProgram $out/bin/robstat \
  #     --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
  # '';
}
