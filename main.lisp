(in-package :robstat)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *colors*
  '(:white  "#FFFFFF"
    :red    "#FF0000"
    :orange "#FFA500"
    :yellow "#FFFF00"
    :green  "#00FF00"
    :blue   "#0000FF"
    :purple "#800080"))

(defvar *dbus*)

(defvar *status-condition* (bt2:make-condition-variable))
(defvar *status-lock* (bt2:make-lock))

(defvar *memo-vars* ())

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
    (yason:encode (mapcar (compose #'to-i3bar #'ensure-item) args))))

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
  (let ((sec (battery-seconds)))
    (fmt "~d:~2,'0d"
         (floor (/ sec 3600))
         (floor (mod (/ sec 60) 60)))))

(defun battery-level ()
  (let ((stats (battery-stats)))
    (/ (battery-stat "POWER_SUPPLY_CHARGE_NOW" stats)
       1.0
       (battery-stat "POWER_SUPPLY_CHARGE_FULL_DESIGN" stats))))

(defun battery-status ()
  (cdr (assoc "POWER_SUPPLY_STATUS" (battery-stats) :test #'equal)))

(defun battery-color ()
  (if (< (battery-seconds) (* 30 60))
      :red
      :white))

(defun battery ()
  (item (fmt "BAT ~a% ~a" (floor (* (battery-level) 100)) (battery-time))
        :color (battery-color))
  (let ((level (floor (* (battery-level) 100))))
    (match (battery-status)
      ("Discharging"
       (item (fmt "BAT ~a% ~a" level (battery-time))
             :color (battery-color)))
      ("Charging"
       (item (fmt "CHG ~a% ~a" level (battery-time))))
      ("Full"
       (item (fmt "FULL ~a%" level)))
      (_
       (item (fmt "UNK ~a%" level))))))

;;;; Networking

;; -> (list (name type dbus-path))
(defun connections ()
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
           :color :red))))

;;;; Load

(defun system-load-1min ()
  (c-with ((x :double))
    (getloadavg (x &) 1)
    (x 0)))

(defvar +nprocs+ (sysconf +_sc_nprocessors_onln+))

(defun system-load ()
  (let ((load (system-load-1min)))
    (item (fmt "~,1f" load)
          :color (if (> load +nprocs+)
                     :red
                     :white))))

;;;; Disk Usage

(defun disk-free-bytes (path)
  (declare (type string path))
  (memoize-status (path)
    (c-with ((info (:struct (robstat.c:statvfs))))
      (statvfs path info)
      (* (info :f-bavail) (info :f-bsize)))))

(defun si-round (n)
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
    (item (fmt "~a: ~a" path (si-round free))
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

;;; -> '(:volume [0.0-1.0] :muted bool)
(defun volume-level ()
  (c-with ((minv :long)
           (maxv :long)
           (vol :long)
           (muted :int))
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
          :muted (= muted 0))))

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
             (format *error-output* "Volume thread listening...~%")
             (loop
              (progn
                (snd-mixer-wait handle -1)
                (snd-mixer-handle-events handle)
                (bt2:condition-broadcast *status-condition*))))
        (snd-mixer-close handle)))))

;;;; Top-level

;;; Only works for binding dynamic variables; lexical vars will be
;;; unbound since they wouldn't be set in the failure case
(defmacro ignorable-with (with args &body body)
  `(flet ((body () ,@body))
     (let ((entered nil))
       (block outer
         (block inner
           (handler-bind
               ((error
                  #'(lambda (err)
                      (unless entered
                        (format *error-output* "~a failed: ~a~%proceeding without...~%" ',with err)
                        (return-from inner)))))
             (return-from outer
               (,with ,args
                      (setf entered t)
                      (body)))))
         (body)))))

(defun call-with-status-env (f)
  (ignorable-with with-volume ()
    (ignorable-with dbus:with-open-bus (*dbus* (dbus:system-server-addresses))
      (let* ((*error-output* (open #p"~/.robstat.log"
                                   :direction :output
                                   :if-exists :supersede))
             (local-time:*default-timezone*
               (local-time::make-timezone :path #p"/etc/localtime"
                                          :name "localtime"))
             (bt2:*default-special-bindings*
               (cons `(*error-output* . *error-output*)
                     bt2:*default-special-bindings*))
             (vol-thread (bt2:make-thread #'listen-volume :trap-conditions t)))
        (format *error-output* "Robstat started~%")
        (finish-output *error-output*)
        (unwind-protect
             (funcall f)
          (when (bt2:thread-alive-p vol-thread)
            (bt2:interrupt-thread
             vol-thread
             #'(lambda () (error "Kill thread"))))
          (handler-case (bt2:join-thread vol-thread)
            (error (e)
              (format *error-output* "Vol thread joined with ~a~%" e)))
          (close *error-output*))))))

(defmacro with-status-env ((&key) &body body)
  `(call-with-status-env #'(lambda () ,@body)))

(defmacro status (&rest args)
  "Output the given status items on a timer.

   Items can be output from `item', or strings."
  `(with-status-env ()
     (emit-status-version)
     (format t "[")
     (loop while t do
       (progn
         (status-once ,@args)
         (format t ",~%")
         (bt2:with-lock-held (*status-lock*)
           (bt2:condition-wait *status-condition* *status-lock* :timeout 5))))))

(defmacro status-once (&rest args)
  `(with-status-env ()
     (clear-status-memos)
     (emit-status
      ,@(mapcar #'(lambda (arg)
                    `(handler-case ,arg
                       (error (v)
                         (format *error-output* "Error with item ~a: ~a" ',arg v)
                         (item "<error>" :color :red))))
                args))))

(defun test ()
  (status-once
   (wifi)
   (vpn)
   (battery)
   (disk-free "/")
   (system-load)
   ;; (item (fmt "~a | ~a" (memory-used) (memory-available)))
   (volume)
   (local-time:format-timestring
    nil (local-time:now)
    :format '(:short-weekday #\  (:year 4) #\- (:month 2) #\-
              (:day 2) #\  (:hour 2) #\: (:min 2)))))

(defun main ()
  (status
   (wifi)
   (vpn)
   (battery)
   (disk-free "/")
   (system-load)
   ;; (item (fmt "~a | ~a" (memory-used) (memory-available)))
   (volume)
   (local-time:format-timestring
    nil (local-time:now)
    :format '(:short-weekday #\  (:year 4) #\- (:month 2) #\-
              (:day 2) #\  (:hour 2) #\: (:min 2)))))
