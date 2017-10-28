;;;
;;; Test fluent-logger
;;;

(use gauche.test)

(test-start "fluent-logger")

(test-section "fluent-logger")
(use fluent-logger)
(test-module 'fluent-logger)

(test-section "fluent-logger.sender")
(use fluent-logger.sender)
(test-module 'fluent-logger.sender)

(test-section "fluent-logger.sender.socket-sender")
(use fluent-logger.sender.socket-sender)
(test-module 'fluent-logger.sender.socket-sender)

(use gauche.logger)
(use gauche.net)
(use gauche.threads)
(use gauche.uvector)
(use rfc.json)

(let ()
  (define port 24232)
  (define buffer-size 32)
  (define (test-server sock oport)
    (let ((client #f))
      (values
       (lambda () client)
       (make-thread
        (lambda ()
          (while (socket-accept sock) => s
            (print 'reconnect)
            (set! client s)))))))
  (define (sock-copy sock oport)
    (let ((fds (sys-fdset (socket-fd sock))))
      (let loop ()
        (cond ((sys-select fds #f #f (list 2 0))
               (lambda (n . _)
                 (> n 0))
               => (lambda _
                    (display (socket-recv sock 32) oport)
                    (loop)))
              (else
               #f)))))
  (define (test-session)
    (let* ((sender (make-fluent-logger-inet-socket-sender
                    :log-drain (make <log-drain> :path #f)
                    :port port))
           (logger (make <fluent-logger>
                     :tag "test"
                     :sender sender))
           (oport (open-output-string))
           (server-socket (make-server-socket 'inet port :reuse-addr? #t)))
      (receive (get-sock server-thread) (test-server server-socket oport)
        (unwind-protect
            (begin
              (thread-start! server-thread)
              (fluent-logger-log logger "x" 0 :true #t)
              (fluent-logger-log logger "y" 1 :second 2 :third 3)
              (fluent-logger-log logger "z" 2 :foobar "baz")
              (sys-sleep 1)
              (let ((s (get-sock)))
                (sock-copy (get-sock) oport)
                (socket-close s))
              (sys-sleep 3)
              (fluent-logger-log logger "w" 3 :array '#(1 2 3))
              (fluent-logger-flush logger)
              (sock-copy (get-sock) oport)
              (fluent-logger-close logger)
              (thread-terminate! server-thread)
              (get-output-string oport))
          (begin
            (socket-close server-socket))))))
  (test* "fluent-logger-log"
         (with-output-to-string
           (lambda ()
             (for-each construct-json
                       '(
                         #("test.x" 0 (("true" . #t)))
                         #("test.y" 1 (("second" . 2)
                                          ("third" . 3)))
                         #("test.z" 2 (("foobar" . "baz")))
                         ;; #("test.w" 3 (("array" . #(1 2 3))))
                         ))))
         (test-session))
  )

(let* ((handler-called #t)
       (sender (make-fluent-logger-inet-socket-sender
                :host "localhost"
                :log-drain (make <log-drain> :path #f)
                :buffer-capacity 0
                :buffer-overflow-handler (lambda (sender buffer msg)
                                           (set! handler-called #t)))))
  (test* "call buffer-overflow-handler"
         #t
         (let ()
           (fluent-logger-sender-write sender "foobar")
           handler-called)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
