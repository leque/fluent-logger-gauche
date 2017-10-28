;; Copyright (c) 2017 OOHASHI Daichi <dico.leque.comicron@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(define-module fluent-logger.sender.socket-sender
  (export make-fluent-logger-inet-socket-sender)
  (use gauche.fcntl)
  (use gauche.logger)
  (use gauche.net)
  (use gauche.uvector)
  (use gauche.vport)
  (use srfi-19)
  (use binary.ftype)
  (use fluent-logger.sender))

(select-module fluent-logger.sender.socket-sender)

(define (%log msg sender exc)
  (log-format (~ sender 'log-drain)
              "~A: ~S: ~A"
              msg
              sender
              (condition-ref exc 'message)
              ))

;; FIXME: time_t and subseconds_t may not be long.
(define-fstruct-type timeval make-timeval timeval?
  ((tv_sec ftype:long)
   (tv_usec ftype:long)))

(define-class <fluent-logger-socket-sender> (<fluent-logger-sender>)
  ((make-socket
    :init-keyword :make-socket)
   (log-drain
    :init-keyword :log-drain)
   (timeout
    :init-keyword :timeout)
   (buffer-capacity
    :init-keyword :buffer-capacity)
   (buffer
    :init-keyword :buffer)
   (buffer-overflow-handler
    :init-keyword :buffer-overflow-handler)
   (last-connection-error-time
    :init-value #f)
   (retry-count
    :init-value 0)
   (socket
    :init-value #f)
   ))

(define-class <fluent-logger-inet-socket-sender> (<fluent-logger-socket-sender>)
  ((host
    :init-keyword :host)
   (port
    :init-keyword :port)))

(define (%sock-status s)
  (cond (s => socket-status)
        (else #f)))

(define-method write-object ((sender <fluent-logger-inet-socket-sender>) port)
  (format port "#<fluent-logger-inet-socket-sender ~A:~A (~A)>"
          (~ sender 'host)
          (~ sender 'port)
          (%sock-status (~ sender 'socket))))

(define default-timeout (* 3 1000))

(define default-buffer-capacity (* 8 1024 1024))

(define (make-fluent-logger-inet-socket-sender
         :key
         (host "localhost")
         (port 24224)
         (log-drain (make <log-drain>))
         (timeout default-timeout)
         (buffer-capacity default-buffer-capacity)
         (buffer-overflow-handler values))
  (let ((buf (make-u8vector buffer-capacity)))
    (make <fluent-logger-inet-socket-sender>
      :host host
      :port port
      :make-socket (lambda ()
                     (make-client-socket 'inet host port))
      :log-drain log-drain
      :timeout timeout
      :buffer (open-output-uvector buf :extendable #f)
      :buffer-capacity buffer-capacity
      :buffer-overflow-handler buffer-overflow-handler)))

(define-method fluent-logger-sender-write ((sender <fluent-logger-socket-sender>) msg)
  (let* ((bufport (~ sender 'buffer))
         (uvec (get-output-uvector bufport :shared #t))
         (pos (port-tell bufport))
         (sz (string-size msg)))
    (cond
     ((> sz (- (~ sender 'buffer-capacity) pos))
      (log-format (~ sender 'log-drain)
                  "[WARN] buffer overflow. Cannot send logs to server: ~S"
                  sender)
      ((~ sender 'buffer-overflow-handler) sender bufport msg)
      #f)
     (else
      (display msg bufport)
      (%sender-flush sender)))))

(define-method fluent-logger-sender-close ((sender <fluent-logger-socket-sender>))
  (%sender-flush sender)
  (%sender-close sender))

(define-method fluent-logger-sender-flush ((sender <fluent-logger-socket-sender>))
  (%sender-flush sender))

(define-syntax ignore-errors
  (syntax-rules ()
    ((_ expr ...)
     (guard (exc
             (else #f))
       expr ...))))

(define (%sender-close sender)
  (if-let1 socket (~ sender 'socket)
    (begin
      (set! (~ sender 'socket) #f)
      (ignore-errors (socket-shutdown socket SHUT_RDWR))
      (ignore-errors (socket-close socket))
      #t)))

(define max-retry-interval 60)

(define (%ensure-connection sender thunk)
  (define (check-connection)
    (let ((socket (~ sender 'socket)))
      (when socket
        (let* ((fd (socket-fd socket))
               (flag (sys-fcntl fd F_GETFL)))
          (guard (exc
                  ((condition-has-type? exc <system-error>)
                   (unless (= EAGAIN (condition-ref exc 'errno))
                     (%log "[WARN] lost connection" sender exc)
                     (%sender-close sender))))
            (unwind-protect
                (begin
                  (sys-fcntl fd F_SETFL (logior flag O_NONBLOCK))
                  (socket-recv socket 1)
                  #t)
              (sys-fcntl fd F_SETFL flag)))))))
  (define (require-connection?)
    (let ((last (cond ((~ sender 'last-connection-error-time)
                       => time-second)
                      (else
                       #f))))
      (or (not last)
          (let ((now (time-second (current-time)))
                (interval (min max-retry-interval
                               (expt 2 (~ sender 'retry-count)))))
            (< interval (- now last))))))
  (define (connect)
    (let* ((socket ((~ sender 'make-socket)))
           (timeout-sec (~ sender 'timeout))
           (sec (exact (floor timeout-sec)))
           (usec (exact (floor (* 1e6 (- timeout-sec sec)))))
           (timeval (make-timeval :tv_sec sec
                                  :tv_usec usec)))
      (socket-setsockopt socket
                         SOL_SOCKET
                         SO_SNDTIMEO
                         (fobject-storage timeval))
      (set! (~ sender 'socket) socket)
      (set! (~ sender 'last-connection-error-time) #f)
      (set! (~ sender 'retry-count) 0)
      (log-format (~ sender 'log-drain) "[INFO] connect to host: ~A" sender)))
  (define (record-connection-error)
    (set! (~ sender 'socket) #f)
    (set! (~ sender 'last-connection-error-time) (current-time))
    (inc! (~ sender 'retry-count)))
  (check-connection)
  (cond
   ((~ sender 'socket)
    (thunk))
   (else
    (and (require-connection?)
         (guard (exc
                 ((condition-has-type? exc <system-error>)
                  (%log "[WARN] cannot connect to host" sender exc)
                  (record-connection-error)
                  #f))
           (connect)
           #t)
         (thunk)))))

(define (%sender-flush sender)
  (%ensure-connection
   sender
   (lambda ()
     (let* ((bufport (~ sender 'buffer))
            (pos (port-tell bufport)))
       (when (positive? pos)
         (guard (exc
                 ((condition-has-type? exc <system-error>)
                  (%log "[WARN] error while flushing buffer" sender exc)
                  (%sender-close sender)
                  #f))
           (let ((uvec (u8vector-copy
                        (get-output-uvector bufport :shared #t)
                        0 pos)))
             (begin0
                 (socket-send (~ sender 'socket) uvec)
               (port-seek bufport 0)))))))))
