#lang web-server

(require web-server/servlet-env
         web-server/dispatch
         net/http-client
         json
         web-server/http/request-structs)

(require "nhl.rkt")

(provide nhl-bot current-webhook-host current-webhook-uri)

(define current-webhook-host (make-parameter #f))
(define current-webhook-uri (make-parameter #f))

(define (handle-root req)
  (response/xexpr `(html (body "HI"))))

(define (handle-events req)
  (displayln (request-post-data/raw req))
  (define-values (status headers in)
    (http-sendrecv (current-webhook-host) (current-webhook-uri)
                   #:ssl? #t #:method "POST"
                   #:data (jsexpr->bytes (make-hash '((text . "IMADUDE"))))))
  (response/full 200 #"OK (current-seconds" #f '() '()))

(define (slack-events req)
  (define event-data (bytes->jsexpr (request-post-data/raw req)))
  (define challenge-response (response/full
                              200 #"OK" (current-seconds) #"application/json" '()
                              (list (jsexpr->bytes (make-hash `((challenge . ,(hash-ref event-data 'challenge))))))))
  (define bad-callback-response (response/full 400 #"unrecognized event type" (current-seconds) #f '() '()))

  (match (hash-ref event-data 'type)
    ("url_verification"  challenge-response)
    ("event_callback" (handle-events req))
    (_ bad-callback-response)))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("slack-events") #:method "post" slack-events)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))
