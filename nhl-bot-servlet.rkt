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

(define (slack-events req)
  (define event-data (bytes->jsexpr (request-post-data/raw req)))
  (define challenge-response (response/full
                              300 #"OK" (current-seconds) #"application/json" '()
                              (list (jsexpr->bytes (make-hash `((challenge . ,(hash-ref event-data 'challenge))))))))
  (define bad-callback-response (response/full 400 #"unrecognized event type" (current-seconds) #f '() '()))

  (match (string->symbol (hash-ref event-data 'type))
    (url_verification challenge-response)
    (event_callback (handle-events req))

    (_ bad-callback-response)))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("slack-events") #:method "post" slack-events)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))
