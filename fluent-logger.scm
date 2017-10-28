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

(define-module fluent-logger
  (export <fluent-logger>
          fluent-logger-log fluent-logger-flush fluent-logger-close)
  (use srfi-1)
  (use srfi-19)
  (use rfc.json)
  (use util.match)
  (use fluent-logger.sender)
  )

(select-module fluent-logger)

(define-class <fluent-logger> ()
  ((tag :init-keyword :tag)
   (sender :init-keyword :sender)))

(define (fluent-logger-log logger label . args)
  (receive (timestamp plist)
      (if (or (null? args) (keyword? (car args)))
          (values #f args)
          (car+cdr args))
    (let* ((sender (~ logger 'sender))
           (timestamp (cond ((integer? timestamp)
                             timestamp)
                            ((time? timestamp)
                             (time-second timestamp))
                            ((eq? timestamp #f)
                             (time-second (current-time)))
                            (else
                             (error "integer, time, or #f required, but got" timestamp))))
           (tag (format "~A.~A" (~ logger 'tag) label))
           (data (kvlist->alist plist))
           (msg (construct-json-string
                 (vector tag timestamp data))))
      (when msg
        (fluent-logger-sender-write sender msg)))))

(define (kvlist->alist ps)
  (let loop ((rs '())
             (ps ps))
    (match ps
      (()
       (reverse! rs))
      (((? keyword? k) v . rest)
       (loop (alist-cons (keyword->string k) v rs) rest))
      (_
       (error "keyword list is not even:" ps)))))

(define (fluent-logger-flush logger)
  (fluent-logger-sender-flush (~ logger 'sender)))

(define (fluent-logger-close logger)
  (fluent-logger-flush logger)
  (fluent-logger-sender-close (~ logger 'sender)))
