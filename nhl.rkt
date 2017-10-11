#lang racket

(require net/url
         net/cookies
         net/uri-codec
         net/head
         net/url-connect
         (prefix-in gregor: gregor)
         json)

(define current-user-agent (make-parameter "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:49.0) Gecko/20100101 Firefox/49.0"))
; NOTE: This token is from the meta tag "control_plane_client_token" on https://www.nhl.com/login
(define current-auth-token (make-parameter "d2ViX25obC12MS4wLjA6MmQxZDg0NmVhM2IxOTRhMThlZjQwYWM5ZmJjZTk3ZTM="))
(define current-email (make-parameter #f))
(define current-pw (make-parameter #f))
(define platform "IPHONE")

; globally define this so we can fetch the auth key from the cookie jar
(define login-url (url "https" #f "user.svc.nhl.com" #f #t
                       (map (λ (path) (path/param path '())) '("v2" "user" "identity"))
                       '() #f))

(define (get-session-key game-pk event-id media-playback-id)
  (define session-header (list "Accept: application/json"
                               "Accept-Encoding: identity"
                               "Accept-Language: en-US,en;q=0.8"
                               "Connection: keep-alive"
                               (~a "User-Agent: " (current-user-agent))
                               (~a "Cookie: " (cookie-header login-url))
                               "Origin: https://www.nhl.com"
                               (~a "Referer: https://www.nhl.com/tv/" game-pk "/" event-id "/" media-playback-id)))
    (define session-url (url "https" #f "mf.svc.nhl.com" #f #t
                             (map (λ (path) (path/param path '())) '("ws" "media" "mf" "v2.4" "stream"))
                             `((eventId . "221-1006640")
                               (format . "json")
                               (platform . ,platform)
                               (subject . "NHLTV")
                               (_ . ,(number->string (current-milliseconds))))
                             #f))

    (parameterize ((current-https-protocol 'secure))
      (call/input-url session-url get-pure-port (compose string->jsexpr port->string) session-header)))

(define scheduled-games
  (λ (#:date (game-day (gregor:today)))
    (define expand-params "schedule.teams,schedule.linescore,schedule.scoringplays,schedule.game.content.media.epg")
    (define schedule-header (list "Connection: close"
                                  "User-Agent: UA_PS4"))
    (define schedule-url (url "https" #f "statsapi.web.nhl.com" #f #t
                              (map (λ (path) (path/param path '())) '("api" "v1" "schedule"))
                              `((expand . ,expand-params)
                                (platform . ,platform)
                                (site . "en_nhl")
                                (date . ,(gregor:date->iso8601 game-day)))
                              #f))

    (call/input-url schedule-url get-pure-port (compose string->jsexpr port->string) schedule-header)))

(define (nhl-logout)
  (define logout-url (url "https" #f "account.nhl.com" #f #t
                          (map (λ (path) (path/param path '())) '("ui" "rest" "logout"))
                          '() #f))

  (define logout-header (list "Accept: */*"
                              "Accept-Encoding: gzip, deflate, sdch"
                              "Accept-Language: en-US,en;q=0.8"
                              "Content-Type: application/x-www-form-urlencoded"
                              "Origin: https://account.nhl.com/ui/SignOut?lang=en"
                              "Connection: close"
                              (~a "User-Agent: " (current-user-agent))))

  (define-values (logout-response-header logout-response)
    (call/input-url logout-url
                    (λ (u h)
                      (post-impure-port u #"" h))
                    (λ (p)
                      (values (purify-port p)
                              (port->string p)))
                    logout-header))

  (values logout-response-header logout-response))


(define (nhl-authenticate)
  (unless (and (current-email) (current-pw))
    (raise-user-error "No username or pasword set."))

  (define token-url (url "https" #f "user.svc.nhl.com" #f #t
                         (map (λ (path) (path/param path '())) '("oauth" "token"))
                         '((grant_type . "client_credentials")) #f))

  (define token-header (list "Accept: application/json"
                             "Accept-Encoding: gzip, deflate, sdch"
                             "Accept-Language: en-US,en;q=0.8"
                             "Origin: https://www.nhl.com"
                             (~a "Authorization: Basic " (current-auth-token))
                             (~a "User-Agent: " (current-user-agent))))

  (define-values (token-response-header token-response)
    (call/input-url token-url
                    (λ (u h)
                      (post-impure-port u #"" h))
                    (λ (p)
                      (values (purify-port p)
                              ((compose string->jsexpr port->string) p)))
                    token-header))

  (parameterize ((current-auth-token (hash-ref token-response 'access_token)))


    (define auth-header (list (~a "Authorization: " (current-auth-token))
                              (~a "User-Agent: " (current-user-agent))
                              "Accept: */*"
                              "Accept-Encoding: gzip, deflate"
                              "Accept-Language: en-US,en;q=0.8"
                              "Content-Type: application/json"))

    (define auth-data (string->bytes/utf-8
                       (jsexpr->string
                        (make-hasheq `((email . ,(make-hasheq `((address . ,(current-email)))))
                                       (password . ,(make-hasheq `((value . ,(current-pw)))))
                                       (type . "email-password"))))))

    (define HTTP-OK #px"HTTP/1.1 200 OK\r\n")

    (define-values (auth-response-header auth-response)
      (call/input-url login-url
                      (λ (u h)
                        (post-impure-port u auth-data h))
                      (λ (p)
                        (let ((head (purify-port p)))
                          (if (regexp-match? HTTP-OK head)
                              (values (extract-all-fields (string->bytes/utf-8 (string-trim head HTTP-OK)))
                                      ((compose string->jsexpr port->string) p))
                              (raise "Login failed"))))
                      auth-header))

    (extract-and-save-cookies! auth-response-header login-url)

    (values auth-response-header auth-response)))
