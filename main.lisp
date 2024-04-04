(in-package :robstat)

(defvar *colors*
  '(:white  "#FFFFFF"
    :red    "#FF0000"
    :orange "#FFA500"
    :yellow "#FFFF00"
    :green  "#00FF00"
    :blue   "#0000FF"
    :purple "#FF00FF"))

(defvar *dbus*)

(defvar *status-condition*)
(defvar *status-lock*)

(defvar *memo-vars* ())

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
  (yason:with-output (*standard-output*)
    (yason:encode-plist '("version" 1)))
  (format t "~%"))

(defun emit-status (&rest args)
  (yason:with-output (*standard-output*)
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
       (item (fmt "BAT ~a% ~a" level (battery-time))
             :color (if (< (battery-seconds) (* 30 60))
                        :red
                        :white)))
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
  "Info for the first wifi card, as (:strength 0-100 :ssid \"weefee\" :state :activated :address \"1.2.3.4\""
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
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    wifi-conn
                    "org.freedesktop.NetworkManager.Connection.Active"
                    "State"))
                 (ip4-config
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    wifi-conn
                    "org.freedesktop.NetworkManager.Connection.Active"
                    "Ip4Config"))
                 (addresses
                   (dbus:get-property
                    *dbus* "org.freedesktop.NetworkManager"
                    ip4-config
                    "org.freedesktop.NetworkManager.IP4Config"
                    "AddressData")))
            (list :strength strength
                  :ssid ssid
                  :state (decode-wifi-state state)
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

(defvar +ncores+ (sysconf +_sc_nprocessors_onln+))

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
(defvar *alsa-elem*)

(defun call-with-volume (f)
  (cffi:with-foreign-strings ((master "Master")
                              (card "default"))
    (c-with ((mixid (:pointer snd-mixer-selem-id-t))
             (handle (:pointer snd-mixer-t)))
      (acheck (snd-mixer-open (handle &) 0))
      (unwind-protect
           (progn
             (acheck (snd-mixer-attach handle card))
             (acheck (snd-mixer-selem-register handle nil nil))
             (acheck (snd-mixer-load handle))

             (acheck (snd-mixer-selem-id-malloc (mixid &)))
             (unwind-protect
                  (progn
                    (snd-mixer-selem-id-set-index mixid 0)
                    (snd-mixer-selem-id-set-name mixid master)
                    (let ((elem (snd-mixer-find-selem handle mixid)))
                      (unless (null elem)
                        (funcall f handle elem))))
               (snd-mixer-selem-id-free mixid)))
        (snd-mixer-close handle)))))

(defmacro with-volume ((&optional (handle '*alsa-handle*) (elem '*alsa-elem*)) &body body)
  `(call-with-volume
    #'(lambda (,handle ,elem)
        ,@body)))

(defun volume-level ()
  "Gets the master volume in (list :volume [0.0-1.0] :muted t/nil)"
  (memoize-status ()
    (c-with ((minv :long)
             (maxv :long)
             (vol :long)
             (muted :int))
      (acheck (snd-mixer-handle-events *alsa-handle*))
      (acheck
       (snd-mixer-selem-get-playback-volume-range
        *alsa-elem*
        (minv &)
        (maxv &)))
      (acheck
       (snd-mixer-selem-get-playback-volume
        *alsa-elem* 0
        (vol &)))
      (acheck
       (snd-mixer-selem-get-playback-switch
        *alsa-elem* 0
        (muted &)))

      (list :volume (/ (- vol minv 0.0) (- maxv minv))
            :muted (= muted 0)))))

(defun volume ()
  (let-match* (((plist :volume vol :muted muted) (volume-level))
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

(defun listen-volume ()
  (cffi:with-foreign-string (card "default")
    (c-with ((handle (:pointer snd-mixer-t)))
      (acheck (snd-mixer-open (handle &) 0))
      (unwind-protect
           (progn
             (acheck (snd-mixer-attach handle card))
             (acheck (snd-mixer-selem-register handle nil nil))
             ;; (snd-mixer-set-callback handle (autowrap:callback 'mixer-event))
             (acheck (snd-mixer-load handle))
             (logf "Volume thread listening...~%")
             (loop
              (progn
                (snd-mixer-wait handle -1)
                (snd-mixer-handle-events handle)
                (bt2:condition-broadcast *status-condition*))))
        (snd-mixer-close handle)))))

;;;; Bluetooth

(defun bluetooth-devices ()
  "All connected bluetooth devices"
  (memoize-status ()
    (let ((objs (dbus:get-managed-objects *dbus* "org.bluez" "/")))
      (flet ((property (obj interface prop)
               (second
                (assoc prop
                       (second
                        (assoc interface (second obj) :test #'string=))
                       :test #'string=))))
        (mapcar
         (lambda (obj)
           (let ((name (property obj "org.bluez.Device1" "Name"))
                 (battery (property obj "org.bluez.Battery1" "Percentage")))
             (append (list :name name)
                     (when battery (list :battery battery)))))
         (remove-if-not
          (lambda (obj)
            (property obj "org.bluez.Device1" "Connected"))
          objs))))))

(defun bluetooth ()
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
              devices)))))

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

(defun call-with-status-env (f &key (logfile t))
  (ignorable-with with-volume ()
    (ignorable-with dbus:with-open-bus (*dbus* (dbus:system-server-addresses))
      (let* ((*error-output* (if logfile
                                 (open #p"~/.robstat.log"
                                       :direction :output
                                       :if-exists :supersede)
                                 *error-output*))
             (*status-lock* (bt2:make-lock))
             (*status-condition* (bt2:make-condition-variable))
             (local-time:*default-timezone*
               (local-time::make-timezone :path #p"/etc/localtime"
                                          :name "localtime"))
             (vol-thread
               (bt2:make-thread #'listen-volume
                                :trap-conditions t
                                :initial-bindings
                                `((*error-output* . ,*error-output*)
                                  (*status-lock* . ,*status-lock*)
                                  (*status-condition* . ,*status-condition*)))))
        (logf "Robstat started~%")
        (finish-output *error-output*)
        (unwind-protect
             (funcall f)
          (when (bt2:thread-alive-p vol-thread)
            (bt2:interrupt-thread
             vol-thread
             #'(lambda () (error "Kill thread"))))
          (handler-case (bt2:join-thread vol-thread)
            (error (e)
              (logf "Vol thread joined with ~a~%" e)))
          (when logfile (close *error-output*)))))))

(defmacro with-status-env ((&rest args) &body body)
  `(call-with-status-env #'(lambda () ,@body) ,@args))

(defvar *interval* 5 "How long to wait between updates, in seconds")

(defmacro status ((&key) &body items)
  "Output the given status items on a timer.

   Items can be output from `item', or strings."
  `(with-status-env ()
     (emit-status-version)
     (format t "[")
     (loop while t do
       (progn
         (status-once () ,@items)
         (format t ",~%")
         (bt2:with-lock-held (*status-lock*)
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
     ;; (item (fmt "~a | ~a" (memory-used) (memory-available)))
     (volume)
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
   ;; (item (fmt "~a | ~a" (memory-used) (memory-available)))
   (volume)
   (local-time:format-timestring
    nil (local-time:now)
    :format '(:short-weekday #\  (:year 4) #\- (:month 2) #\-
              (:day 2) #\  (:hour 2) #\: (:min 2)))))

(defun main ()
  (let ((config-file
          (uiop:xdg-config-pathname "robstat/stat.lisp")))
    (if (not config-file)
        (progn
          (logf "File $XDG_CONFIG_HOME/robstat/stat.lisp not found, using default~%")
          (finish-output *error-output*)
          (main0))
        (let ((*package* (symbol-package 'robstat-user)))
          (load config-file)))))
