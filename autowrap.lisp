(cl:in-package :robstat)

(cffi:define-foreign-library libasound
  (t (:default "libasound")))

(cffi:use-foreign-library libasound)

(cffi:define-foreign-library libpulse
  (t (:default "libpulse")))

(cffi:use-foreign-library libpulse)

(cl:in-package :robstat.c)

(autowrap:c-include
 "test.h"
 :trace-c2ffi cl:t
 :spec-path #p"./spec/"
 :exclude-definitions (".*")
 :include-definitions ("getloadavg" "size_t" "timeval" "timespec"
                                    "statvfs" "__fsblkcnt_t" "__fsfilcnt_t"
                                    ".*snd_mixer.*" ".*SND_.*"
                                    "sysconf" "_SC_.*"
                                    "pa_.*" "PA_.*" "uint32_t"))
