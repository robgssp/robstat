(in-package :robstat)

;; (declaim (optimize (debug 3)))

(defvar *colors*
  '(:white  "#FFFFFF"
    :red    "#FF0000"
    :orange "#FFA500"
    :yellow "#FFFF00"
    :green  "#00FF00"
    :blue   "#0000FF"
    :purple "#FF00FF"))

(defvar *status-output* *standard-output*)

(defvar *dbus*)

(defvar *status-condition* (bt2:make-condition-variable))
(defvar *status-lock* (bt2:make-lock))

(defvar *memo-vars* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-at (pred list)
    "Split LIST at the first item for which PRED is true;
returns (VALUES LEFT RIGHT)"
    (if (funcall pred (car list))
        (values nil list)
        (multiple-value-bind (left right)
            (split-at pred (cdr list))
          (values (cons (car list) left)
                  right))))

  (defun extract-decls (body)
    "Break BODY into (VALUES DECLS BODY1)"
    (split-at (lambda (exp) (not (eq (car exp) 'declare)))
              body)))

(defmacro bracket ((binding val) cleanup &body body)
  (multiple-value-bind (decls body) (extract-decls body)
    `(let ((,binding ,val))
       ,@decls
       (unwind-protect
            (progn ,@body)
         ,cleanup))))

(defun logf (fmt &rest args)
  (apply #'format *error-output* fmt args)
  (finish-output *error-output*))

(defun make-dbus ()
  (let* ((event-base (make-instance 'iolib:event-base))
         (connection (dbus:open-connection event-base (dbus:system-server-addresses))))
    (dbus:authenticate (dbus:supported-authentication-mechanisms connection) connection)
    (make-instance 'dbus:bus :name (dbus:hello connection) :connection connection)))

;; (defmacro time-expr (&body body)
;;   (with-gensyms (start)
;;     `(let (,start (get-internal-real-time))
;;        (prog1
;;            (progn @,body)
;;          (format *error-output* "Elapsed: " (/ (- (get-internal-real-time) ,start)
;;                                                1.0
;;                                                (internal-time-units-per-second)))))))

(defun status-refresh ()
  (bt2:condition-broadcast *status-condition*))

;;;; Memoization

(defmacro memoize-status ((&rest memo-keys) &body body)
  (let ((memos (gensym "MEMOS-"))
        (memo-key (gensym "MEMO-KEY-"))
        (memo-assoc (gensym "MEMO-ASSOC-"))
        (value (gensym "VALUE-")))
    `(progn
       (let* ((,memos (load-time-value
                       (let ((cell (list nil)))
                         (push cell *memo-vars*)
                         cell)))
              (,memo-key (list ,@memo-keys))
              (,memo-assoc (assoc ,memo-key (car ,memos) :test #'equal)))
         (if ,memo-assoc
             (cdr ,memo-assoc)
             (let ((,value (progn ,@body)))
               (push (cons ,memo-key ,value)
                     (car ,memos))
               ,value))))))

(defun clear-status-memos ()
  (dolist (cell *memo-vars*)
    (setf (car cell) ())))

;;;; Output

(defun emit-status-version ()
  (yason:with-output (*status-output*)
    (yason:encode-plist '("version" 1)))
  (format *status-output* "~%"))

(defun emit-status (&rest args)
  (yason:with-output (*status-output*)
    (yason:encode (mapcar (compose #'to-i3bar #'ensure-item) args))
    nil))

(defun ensure-item (item-like)
  (cond ((listp item-like) item-like)
        (t (item item-like))))

(defun to-i3bar (item)
  (match item
    ((alist (:text . text)
            (:color . color))
     (plist-hash-table
      (list "full_text" text
            "color" (color-lookup color))))))

(defun color-lookup (color)
  (cond
    ((typep color 'symbol) (or (getf *colors* color)
                               (error (fmt "Bad color: ~a" color))))
    ((typep color 'string) color)
    (t (error (fmt "Bad color: ~a"  color)))))

(defun fmt (&rest args)
  (apply #'format nil args))

(defun item (text &key (color :white))
  `((:text . ,text)
    (:color . ,color)))

;;;; Battery

(defun battery-stats ()
  "Collection of battery stats from /sys/class/power_supply/BAT*/uevent"
  (memoize-status ()
    (let* ((batpath (first (directory "/sys/class/power_supply/BAT*/uevent")))
           (batstats (uiop:read-file-lines batpath)))
      (mapcar (lambda (line)
                (let ((strings (uiop:split-string line :max 2
                                                       :separator "=")))
                  (cons (car strings) (cadr strings))))
              batstats))))

(defun battery-stat (stat stats)
    (let ((val (cdr (assoc stat stats :test #'equal))))
               (if val
                   (parse-integer val)
                   (error (fmt "No such stat ~a" stat)))))

(defun battery-seconds ()
  "Get the current battery life in seconds"
  (let ((stats (battery-stats)))
    (let (;; uA -> A
          (current (/ (battery-stat "POWER_SUPPLY_CURRENT_NOW" stats) 1000000.0))
          ;; uAh -> Coulombs (A*s)
          (charge (/ (* (battery-stat "POWER_SUPPLY_CHARGE_NOW" stats) 3600) 1000000.0))
          (full (/ (* (battery-stat "POWER_SUPPLY_CHARGE_FULL" stats) 3600) 1000000.0)))
      ;; POWER_SUPPLY_CHARGE_NOW is in microwatt-hours, joules/3600
      (match (battery-status)
        ("Charging" (/ (- full charge) current))
        (_ (/ charge current))))))

(defun battery-time ()
  "Battery time in hours:minutes"
  (let ((sec (battery-seconds)))
    (fmt "~d:~2,'0d"
         (floor (/ sec 3600))
         (floor (mod (/ sec 60) 60)))))

(defun battery-level ()
  "Battery level in [0.0, 1.0]"
  (let ((stats (battery-stats)))
    (/ (battery-stat "POWER_SUPPLY_CHARGE_NOW" stats)
       1.0
       (battery-stat "POWER_SUPPLY_CHARGE_FULL_DESIGN" stats))))

(defun battery-status ()
  "Battery status: \"Discharging\", \"Charging\", \"Full\" or other"
  (cdr (assoc "POWER_SUPPLY_STATUS" (battery-stats) :test #'equal)))

(defun battery ()
  "Formatted battery status"
  (let ((level (floor (* (battery-level) 100))))
    (match (battery-status)
      ("Discharging"
       (handler-case
           (item (fmt "BAT ~a% ~a" level (battery-time))
                 :color (if (< (battery-seconds) (* 30 60))
                            :red
                            :white))
         (error (_)
           (declare (ignore _))
           (item (fmt "BAT ~a% ???" level)
                 :color :yellow))))
      ("Charging"
       (item (fmt "CHG ~a% ~a" level (battery-time))))
      ("Full"
       (item (fmt "FULL ~a%" level)))
      (_
       (item (fmt "UNK ~a%" level))))))

;;;; Networking

(defun connections ()
  "Active NetworkManager connections, returned as a list of (name type dbus-path) triples."
  (memoize-status ()
    (mapcar (lambda (conn)
              (list (dbus:get-property
                     *dbus* "org.freedesktop.NetworkManager" conn
                     "org.freedesktop.NetworkManager.Connection.Active"
                     "Id")
                    (dbus:get-property
                     *dbus* "org.freedesktop.NetworkManager" conn
                     "org.freedesktop.NetworkManager.Connection.Active"
                     "Type")
                    conn))
            (dbus:get-property
             *dbus* "org.freedesktop.NetworkManager"
             "/org/freedesktop/NetworkManager"
             "org.freedesktop.NetworkManager"
             "ActiveConnections"))))

(defun wifi-info ()
  "Info for the first wifi card, as
  (:strength 0-100 :ssid \"weefee\" :state :activated :address \"1.2.3.4\")"
  (memoize-status ()
    (let* ((wifi-conn
             (third (find-if (lambda (conn)
                               (equal "802-11-wireless"
                                      (second conn)))
                             (connections)))))
      (if (not wifi-conn)
          (list :strength 0
                :ssid ""
                :state :deactivated
                :address "127.0.0.1")
          (let* ((wifi-dev
                   (first
                    (dbus:get-property
                     *dbus* "org.freedesktop.NetworkManager"
                     wifi-conn
                     "org.freedesktop.NetworkManager.Connection.Active"
                     "Devices")))
                 (ap
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    wifi-dev
                    "org.freedesktop.NetworkManager.Device.Wireless"
                    "ActiveAccessPoint"))
                 (strength
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    ap
                    "org.freedesktop.NetworkManager.AccessPoint"
                    "Strength"))
                 (ssid
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    wifi-conn
                    "org.freedesktop.NetworkManager.Connection.Active"
                    "Id"))
                 (state
                   (decode-wifi-state
                    (dbus:get-property
                     *dbus* "org.freedesktop.NetworkManager"
                     wifi-conn
                     "org.freedesktop.NetworkManager.Connection.Active"
                     "State")))
                 (addresses
                   (when (eq state :activated)
                     (dbus:get-property
                      *dbus* "org.freedesktop.NetworkManager"
                      (dbus:get-property
                       *dbus* "org.freedesktop.NetworkManager"
                       wifi-conn
                       "org.freedesktop.NetworkManager.Connection.Active"
                       "Ip4Config")
                      "org.freedesktop.NetworkManager.IP4Config"
                      "AddressData"))))
            (list :strength strength
                  :ssid ssid
                  :state state
                  :address (second (assoc "address" (first addresses)
                                          :test #'equal))))))))

(defun vpn ()
  (let* ((conns (connections))
         (vpns (mapcar #'first
                       (remove-if-not
                        (lambda (conn)
                          (member (second conn) '("wireguard" "vpn")
                                  :test #'equal))
                        conns))))
    (if vpns
        (item (fmt "~{~a~^, ~}" vpns))
        (item "No VPNs" :color :yellow))))

(defun decode-wifi-state (state)
  "Wifi state can be unknown, activating, activated, deactivating or deactivated"
  (case state
    (0 :unknown)
    (1 :activating)
    (2 :activated)
    (3 :deactivating)
    (4 :deactivated)
    (otherwise (error (fmt "Unknown wifi state ~a" state)))))

(defun wifi ()
  (match (wifi-info)
    ((plist :state :activated
            :strength strength
            :address addr
            :ssid ssid)
     (item (fmt "W: (~2,'0d% on ~a) ~a" strength ssid addr)
           :color (if (>= strength 50) :green :yellow)))
    ((plist :state _)
     (item (fmt "W: down")
           :color :orange))))

;;;; Load

(defun system-load-1min ()
  (c-with ((x :double))
    (getloadavg (x &) 1)
    (x 0)))

(defparameter +ncores+ (sysconf +_sc_nprocessors_onln+))

(defun system-load (&key (warn +ncores+) (advise (* +ncores+ 0.75)))
  (let ((load (system-load-1min)))
    (item (fmt "~,1f" load)
          :color (cond ((> load warn)
                        :red)
                       ((> load advise)
                        :yellow)
                       (t :white)))))

;;;; Disk Usage

(defun disk-free-bytes (path)
  (declare (type string path))
  (memoize-status (path)
    (c-with ((info (:struct (robstat.c:statvfs))))
      (statvfs path info)
      (* (info :f-bavail) (info :f-bsize)))))

(defun round-bytes (n)
  (macrolet
      ((cases (&rest args)
         `(cond
            ,@(loop for (limit suffix) in args
                    collecting
                    `((>= n ,limit)
                      (fmt "~,1f~a" (/ n 1.0 ,limit) ,suffix)))
            (t (fmt "~dB" n)))))
    (cases
     ((ash 1 40) "T")
     ((ash 1 30) "G")
     ((ash 1 20) "M")
     ((ash 1 10) "K"))))

(defun disk-free (path)
  (let ((free-red (* 25 (ash 1 30)))
        (free (disk-free-bytes path)))
    (item (fmt "~a: ~a" path (round-bytes free))
          :color (if (< free free-red)
                     :red
                     :white))))

;;;; Volume

(defmacro acheck (func)
  (with-gensyms (ret)
    `(let ((,ret ,func))
       (if (>= ,ret 0)
           ,ret
           (error "~a failed with ~a" ',(first func) ,ret)))))

(defvar *alsa-handle*)

(defun call-with-volume-alsa (f)
  (bracket (thread
            (bt2:make-thread
             (lambda () (listen-volume-alsa))
             :name "alsa listener"
             :trap-conditions t))
           (progn
             (bt2:interrupt-thread
              thread
              (lambda () (error "Kill ALSA Thread")))
             (handler-case
                 (bt2:join-thread thread)
               (error (e)
                 (logf "Volume listener thread failed with: ~a~%" e))))

    (cffi:with-foreign-string (card "default")
      (c-with ((mixid (:pointer snd-mixer-selem-id-t))
               (handle (:pointer snd-mixer-t)))
        (bracket (_ (acheck (snd-mixer-open (handle &) 0)))
                 (snd-mixer-close handle)
          (declare (ignore _))

          (acheck (snd-mixer-attach handle card))
          (acheck (snd-mixer-selem-register handle nil nil))
          (acheck (snd-mixer-load handle))

          (funcall f handle))))))

(defmacro with-volume-alsa ((&optional (handle '*alsa-handle*)) &body body)
  `(call-with-volume-alsa
    #'(lambda (,handle)
        ,@body)))

(defun volume-level-alsa ()
  "Gets the master volume in (list :volume [0.0-1.0] :muted t/nil)"
  (memoize-status ()
    (cffi:with-foreign-string (master "Master")
      (c-with ((minv :long)
               (maxv :long)
               (vol :long)
               (muted :int)
               (mixid (:pointer snd-mixer-selem-id-t)))
        (bracket (_ (acheck (snd-mixer-selem-id-malloc (mixid &))))
                 (snd-mixer-selem-id-free mixid)
          (declare (ignore _))

          (snd-mixer-selem-id-set-index mixid 0)
          (snd-mixer-selem-id-set-name mixid master)
          (let ((elem (snd-mixer-find-selem *alsa-handle* mixid)))
            (assert (not (autowrap:wrapper-null-p elem)) (elem)
                    "snd_mixer_find_selem() returned null")

            (acheck (snd-mixer-handle-events *alsa-handle*))
            (acheck
             (snd-mixer-selem-get-playback-volume-range
              elem
              (minv &)
              (maxv &)))
            (acheck
             (snd-mixer-selem-get-playback-volume
              elem 0
              (vol &)))
            (acheck
             (snd-mixer-selem-get-playback-switch
              elem 0
              (muted &)))

            (list :volume (/ (- vol minv 0.0) (- maxv minv))
                  :muted (= muted 0))))))))

(defun volume-alsa ()
  (let-match* (((plist :volume (and vol (type number)) :muted muted)
                (volume-level-alsa))
               (pvol (round (* vol 100))))
              (if muted
                  (item (fmt "Muted ~d%" pvol) :color :yellow)
                  (item (fmt "~d%" pvol)))))

;; (autowrap:defcallback mixer-event :int ((mixer (:pointer snd-mixer-t))
;;                                         (mask :unsigned-int)
;;                                         (elem (:pointer snd-mixer-elem-t)))
;;   (declare (ignore mixer mask elem))
;;   (format t "Received event~%")
;;   0)

(defun listen-volume-alsa ()
  (cffi:with-foreign-string (card "default")
    (c-with ((handle (:pointer snd-mixer-t)))
      (bracket (_ (acheck (snd-mixer-open (handle &) 0)))
               (snd-mixer-close handle)
        (declare (ignore _))

        (acheck (snd-mixer-attach handle card))
        (acheck (snd-mixer-selem-register handle nil nil))
        ;; (snd-mixer-set-callback handle (autowrap:callback 'mixer-event))
        (acheck (snd-mixer-load handle))
        (logf "Volume thread listening...~%")
        (loop
         (progn
           (snd-mixer-wait handle -1)
           (snd-mixer-handle-events handle)
           (status-refresh)))))))

;;;; Pulseaudio Volume

(autowrap:define-bitmask 'pa-context-flags
    `((:noflags . ,+pa-context-noflags+)
      (:noautospawn .  ,+pa-context-noautospawn+)
      (:nofail . ,+pa-context-nofail+)))

(autowrap:define-bitmask-from-constants (pa-subscription-mask)
  +pa-subscription-mask-null+
  +pa-subscription-mask-sink+
  +pa-subscription-mask-source+
  +pa-subscription-mask-sink-input+
  +pa-subscription-mask-source-output+
  +pa-subscription-mask-module+
  +pa-subscription-mask-client+
  +pa-subscription-mask-sample-cache+
  +pa-subscription-mask-server+
  +pa-subscription-mask-card+
  +pa-subscription-mask-all+)

(autowrap:define-bitmask-from-constants (pa-sink-flags)
  +pa-sink-noflags+
  +pa-sink-hw-volume-ctrl+
  +pa-sink-latency+
  +pa-sink-hardware+
  +pa-sink-network+
  +pa-sink-hw-mute-ctrl+
  +pa-sink-decibel-volume+
  +pa-sink-flat-volume+
  +pa-sink-dynamic-latency+
  +pa-sink-set-formats+)

(defvar *pa-context*)
(defvar *pa-volume* 0)
(defvar *pa-muted-p* nil)
(defvar *pa-flags* ())

(defmacro pacheck (call)
  (let ((ret (gensym)))
    `(ematch ,call
       ((and ,ret (type number))
        (assert (>= ,ret 0) () "~a failed: ~a" ',(car call) (pa-strerror ,ret))
        ,ret)
       ((and ,ret (type autowrap:wrapper))
        (assert (not (autowrap:wrapper-null-p ,ret)) ()
                "~a failed: ~a"
                ',(car call)
                (when (boundp '*pa-context*)
                  (pa-strerror (pa-context-errno *pa-context*))))
        ,ret))))

(defmacro pa-op (call)
  (let ((op (gensym)))
    `(let ((,op (pacheck ,call)))
       (pa-operation-unref ,op)
       nil)))

(autowrap:defcallback pulse-state-callback :void
    ((ctx (:pointer pa-context))
     (userdata :pointer))
  (declare (ignore userdata))

  (match (autowrap:enum-key '(:enum (pa-context-state)) (pa-context-get-state ctx))
    (:ready
     (pa-context-set-subscribe-callback ctx (autowrap:callback 'pulse-subscribe-callback) nil)

     (pa-op (pa-context-subscribe ctx (autowrap:mask 'pa-subscription-mask :server :sink)
                                  nil nil))

     (pa-op (pa-context-get-sink-info-by-name ctx "@DEFAULT_SINK@"
                                              (autowrap:callback 'pulse-sink-info-callback)
                                              nil))
     (logf "Pulseaudio thread listening...~%"))
    (:failed (logf "Pulseaudio initialization failed: ~a~%" (pa-strerror (pa-context-errno ctx))))))

(autowrap:defcallback pulse-subscribe-callback :void
    ((ctx (:pointer pa-context))
     (event (:enum (pa-subscription-event-type)))
     (userdata :pointer))
  (declare (ignore event userdata))

  (pa-op (pa-context-get-sink-info-by-name ctx "@DEFAULT_SINK@"
                                           (autowrap:callback 'pulse-sink-info-callback)
                                           nil)))

(autowrap:defcallback pulse-sink-info-callback :void
    ((ctx (:pointer pa-context))
     (info (:pointer pa-sink-info))
     (eol :int)
     (userdata :pointer))
  (declare (ignore ctx userdata))

  (when (= eol 0)
    (let* ((info (robstat.c::make-pa-sink-info :ptr info)))

      (bt2:with-lock-held (*status-lock*)
        (setf *pa-volume* (pa-cvolume-avg (pa-sink-info.volume info)))
        (setf *pa-muted-p* (not (= 0 (pa-sink-info.mute info))))
        (setf *pa-flags* (autowrap:mask-keywords 'pa-sink-flags (pa-sink-info.flags info))))

      (status-refresh))))

(defun listen-volume-pulse ()
  (bracket (mainloop (pacheck (pa-mainloop-new)))
           (pa-mainloop-free mainloop)
    (let* ((api (pacheck (pa-mainloop-get-api mainloop))))
      (bracket (proplist (pa-proplist-new))
               (pa-proplist-free proplist)
        (pa-proplist-sets proplist "application.name" "robstat")
        (pa-proplist-sets proplist "application.id" "org.robstat")
        (pa-proplist-sets proplist "application.version" "0.69")
        (bracket (*pa-context* (pa-context-new-with-proplist api "robstat" proplist))
                 (pa-context-unref *pa-context*)
          (pa-context-set-state-callback *pa-context* (autowrap:callback 'pulse-state-callback) nil)
          (pa-context-connect *pa-context* nil (autowrap:mask 'pa-context-flags :noautospawn :nofail) api)

          (loop (pacheck (pa-mainloop-iterate mainloop 1 nil))))))))

(defun call-with-volume-pulse (f)
  (bracket (thread
            (bt2:make-thread
             (lambda () (listen-volume-pulse))
             :name "pulse listener"
             :trap-conditions t))
           (progn
             (bt2:interrupt-thread
              thread
              (lambda () (error "Kill Pulse Thread")))
             (handler-case
                 (bt2:join-thread thread)
               (error (e)
                 (logf "Volume listener thread failed with: ~a~%" e))))

    (funcall f)))

(defmacro with-volume-pulse (() &body body)
  `(call-with-volume-pulse
    (lambda () ,@body)))

(defun muted-p-pulse ()
  *pa-muted-p*)

(defun volume-pulse ()
  (let ((db (round (pa-sw-volume-to-linear *pa-volume*))))
    (if *pa-muted-p*
        (item (fmt "~a Muted" db) :color :yellow)
        (item (fmt "~a" db)))))

(defun volume-db-pulse ()
  (if (member :decibel-volume *pa-flags*)
      (let ((db (round (pa-sw-volume-to-d-b *pa-volume*))))
        (if *pa-muted-p*
            (item (fmt "~adB Muted" db) :color :yellow)
            (item (fmt "~adB" db))))
      (volume-pulse)))


;;;; Bluetooth

(defun any (list)
  (reduce (lambda (a b) (or a b))
          list
          :initial-value nil))

(defun property (obj interface prop)
  "Extract properties out of get-managed-objects' alist-of-alists repr"
  (second
   (assoc prop
          (second
           (assoc interface (second obj) :test #'string=))
          :test #'string=)))

(defun bluetooth-info ()
  "BlueZ's info structure"
  (memoize-status ()
    (dbus:get-managed-objects *dbus* "org.bluez" "/")))

(defun bluetooth-enabled ()
  (if (any (remove-if-not
            (lambda (obj)
              (property obj "org.bluez.Adapter1" "Powered"))
            (bluetooth-info)))
      t
      nil))

(defun bluetooth-devices ()
  "All connected bluetooth devices"
  (memoize-status ()
    (mapcar
     (lambda (obj)
       (let ((name (property obj "org.bluez.Device1" "Name"))
             (battery (property obj "org.bluez.Battery1" "Percentage")))
         (append (list :name name)
                 (when battery (list :battery battery)))))
     (remove-if-not
      (lambda (obj)
        (property obj "org.bluez.Device1" "Connected"))
      (bluetooth-info)))))

(defun bluetooth ()
  (if (not (bluetooth-enabled))
      "_"
      (match (bluetooth-devices)
        (nil "-")
        (devices
         (format nil "~{~a~^, ~}"
                 (mapcar
                  (lambda-match
                    ((plist :name name :battery nil)
                     (format nil "~a"
                             (subseq name 0 (min (length name) 3))))
                    ((plist :name name :battery battery)
                     (format nil "~a ~a%"
                             (subseq name 0 (min (length name) 3))
                             battery)))
                  devices))))))

;;;; Memory

(defun memory-stats ()
  (memoize-status ()
    (mapcar (lambda (line)
              (ppcre:register-groups-bind (name (#'parse-integer val) kb)
                  ("^(\\S+):\\s+(\\d+)\\s?(kB)?$" line)
                (cons name (if kb (* val 1024) val))))
            (uiop:read-file-lines "/proc/meminfo"))))

(defun memory-available ()
  (cdr (assoc "MemAvailable" (memory-stats) :test #'equal)))

(defun memory-total ()
  (cdr (assoc "MemTotal" (memory-stats) :test #'equal)))

(defun memory ()
  (item (round-bytes (memory-available))
        :color (if (< (/ (memory-available) (memory-total))
                      0.1)
                   :red
                   :white)))

;;;; Top-level

(defmacro ignorable-with (with args &body body)
  "Attempts to run (with args body); if body is never reached,
   run it without the with. Useful for optional context-building."
  `(flet ((body () ,@body))
     (let ((entered nil))
       (block outer
         (block inner
           (handler-bind
               ((error
                  #'(lambda (err)
                      (unless entered
                        (logf "~a failed: ~a~%proceeding without...~%" ',with err)
                        (return-from inner)))))
             (return-from outer
               (,with ,args
                      (setf entered t)
                      (body)))))
         (body)))))

(defun call-with-status-env (f &key)
  (ignorable-with with-volume-pulse ()
    (ignorable-with dbus:with-open-bus (*dbus* (dbus:system-server-addresses))
      (let* ((local-time:*default-timezone*
               (local-time::make-timezone :path #p"/etc/localtime"
                                          :name "localtime")))

        (logf "Robstat started~%")
        (funcall f)))))

(defmacro with-status-env ((&rest args) &body body)
  `(call-with-status-env #'(lambda () ,@body) ,@args))

(defvar *interval* 5 "How long to wait between updates, in seconds")

(defmacro status ((&key) &body items)
  "Output the given status items on a timer.

   Items can be output from `item', or strings."
  `(with-status-env ()
     (emit-status-version)
     (format *status-output* "[")
     (finish-output *status-output*)
     (loop while t do
       (progn
         (bt2:with-lock-held (*status-lock*)
           (status-once () ,@items)
           (format *status-output* ",~%")
           (bt2:condition-wait *status-condition* *status-lock*
                               :timeout *interval*))))))

(defmacro status-once ((&key) &body items)
  `(progn
     (clear-status-memos)
     (emit-status
      ,@(mapcar #'(lambda (arg)
                    `(handler-case ,arg
                       (error (v)
                         (logf "Error with item ~a: ~a~%" ',arg v)
                         (item "<error>" :color :red))))
                items))))

(defun test ()
  (with-status-env ()
    (status-once ()
     (wifi)
     (vpn)
     (battery)
     (disk-free "/")
     (system-load)
     (memory)
     (volume-alsa)
     (bluetooth)
     (local-time:format-timestring
      nil (local-time:now)
      :format '(:short-weekday #\  (:year 4) #\- (:month 2) #\-
                (:day 2) #\  (:hour 2) #\: (:min 2))))))

(defun main0 ()
  (status ()
   (wifi)
   (vpn)
   (battery)
   (disk-free "/")
   (system-load)
   (memory)
   (volume-db-pulse)
   (local-time:format-timestring
    nil (local-time:now)
    :format '(:short-weekday #\  (:year 4) #\- (:month 2) #\-
              (:day 2) #\  (:hour 2) #\: (:min 2)))))

(defun main ()
  (let ((logfile (open #p"~/.robstat.log"
                       :direction :output
                       :if-exists :supersede)))
    (unwind-protect
         (progn
           (setf *status-output* *standard-output*
                 *standard-output* logfile
                 *error-output* logfile)

           ;; (swank:create-server :port 4005)

           (let ((config-file
                  (uiop:xdg-config-pathname "robstat/stat.lisp")))
             (if (not config-file)
                 (progn
                   (logf "File $XDG_CONFIG_HOME/robstat/stat.lisp not found, using default~%")
                   (main0))
                 (let ((*package* (symbol-package 'robstat-user)))
                   (load config-file)))))
      (close logfile))))
