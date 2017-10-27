#lang web-server

(require web-server/servlet-env
         web-server/dispatch
         net/http-client
         json
         web-server/http/request-structs)

(require "nhl.rkt")

(provide nhl-bot current-webhook-url)

(define current-webhook-url (make-parameter ""))
(define current-bot-id (make-parameter ""))

(define (handle-root req)
  (response/xexpr `(html (body "bonerbonerboner"))))

(define (handle-events req event)
  (define webhook-url (string->url (current-webhook-url)))
  (define ok-response (response/full 200 #"OK" (current-seconds) #f '() '()))

  (unless (or (hash-has-key? event 'bot_id) (hash-has-key? event 'subtype))
    (call/input-url webhook-url
                    (λ (url head)
                      (post-pure-port url (jsexpr->bytes (make-hash `((text . ":poolparty:")))) head))
                    port->string
                    '()))
  ok-response)

(define (challenge-response req event)
  (displayln event (current-error-port))
  (response/full 200 #"OK" (current-seconds) #"application/json" '()
                 (list (jsexpr->bytes (make-hash `((challenge . ,(hash-ref event 'challenge))))))))

(define (slack-events req)
  (define event-data (bytes->jsexpr (request-post-data/raw req)))
  (define bad-callback-response (response/full 400 #"unrecognized event type" (current-seconds) #f '() '()))

  (match (hash-ref event-data 'type)
    ("url_verification"  (challenge-response req event-data))
    ("event_callback" (handle-events req (hash-ref event-data 'event)))
    (_ bad-callback-response)))

(define-values (nhl-bot-dispatch nhl-bot-url)
  (dispatch-rules
   (("") handle-root)
   (("slack-events") #:method "post" slack-events)))

(define (nhl-bot req)
  (nhl-bot-dispatch req))
