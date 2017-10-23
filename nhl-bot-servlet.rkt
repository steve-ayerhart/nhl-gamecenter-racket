#lang web-server

(require web-server/servlet web-server/servlet-env)

(require "nhl.rkt")

(define (handle-root req)
  (response/xexpr `(html (body "HI"))))

(define (handle-schedule req date-str)
  (response/xexpr `(html (body ,date-str))))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("schedule" (string-arg)) handle-schedule)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))

(serve/servlet nhl-bot
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:stateless? #t)
