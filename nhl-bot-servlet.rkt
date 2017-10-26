#lang web-server

(require web-server/servlet-env
         web-server/dispatch
         json
         web-server/http/request-structs)

(require "nhl.rkt")

(provide nhl-bot current-webhook-url)

(define current-webhook-url (make-parameter #f))

(define (handle-root req)
  (response/xexpr `(html (body "HI"))))

(define (handle-events req)
  (response/xexpr `(html (body "HI"))))

(define (slack-event req)
  (define event-data (bytes->jsexpr (request-post-data/raw req)))
  (match (string->symbol (hash-ref event-data 'type))
    (url_verification (response/full
                       200 #"OK" (current-seconds)
                       #"application/json"
                       '()
                       (list (jsexpr->bytes (make-hash `((challenge . ,(hash-ref challenge-data 'challenge))))))))

    (event_callback (handle-events req))

    (_ (response 400 #"unrecoganized event type" (currernt-seconds) #f '() '()))))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("slack-challenge") #:method "post" slack-event)
   (("schedule" (string-arg)) handle-schedule)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))
