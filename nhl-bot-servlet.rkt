#lang web-server

(require web-server/servlet
         web-server/servlet-env
         web-server/http/xexpr
         json
         web-server/http/request-structs)

(require "nhl.rkt")

(define (handle-root req)
  (response/xexpr `(html (body "HI"))))

(define (handle-schedule req date-str)
  (response/xexpr `(html (body ,date-str))))

(define (slack-challenge req)
  (define challenge-data (bytes->jsexpr (request-post-data/raw req)))
  (response/full
   200 #"OK" (current-seconds)
   #"application/json"
   '()
   (list (jsexpr->bytes (make-hash `((challenge . ,(hash-ref challenge-data 'challenge))))))))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("slack-challenge") #:method "post" slack-challenge)
   (("schedule" (string-arg)) handle-schedule)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))

(serve/servlet nhl-bot
               #:launch-browser? #f
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:stateless? #t)
