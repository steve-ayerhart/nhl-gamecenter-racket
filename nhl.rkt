#lang racket

(require net/url
         net/cookies
         net/uri-codec
         net/head
         net/url-connect
         racket/struct
         (prefix-in gregor: gregor)
         json
         (planet neil/json-parsing))

(provide scheduled-games)

(struct team (id name abbreviation team-name location-name short-name site-url)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
            (λ (team-obj) 'team)
            (λ (team-obj) (list (string->symbol (team-abbreviation team-obj))))))))

; globally define this so we can fetch the auth key from the cookie jar
(define login-url (url "https" #f "user.svc.nhl.com" #f #t
                       (map (λ (path) (path/param path '())) '("v2" "user" "identity"))
                       '() #f))
(define current-user-agent (make-parameter "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:49.0) Gecko/20100101 Firefox/49.0"))
; NOTE: This token is from the meta tag "control_plane_client_token" on https://www.nhl.com/login
(define current-auth-token
  (make-parameter (let ((ch (cookie-header login-url)))
                    (if ch
                        (cdr (assoc #"Authorization" (cookie-header->alist ch)))
                        #"d2ViX25obC12MS4wLjA6MmQxZDg0NmVhM2IxOTRhMThlZjQwYWM5ZmJjZTk3ZTM="))))

(define current-email (make-parameter #f))
(define current-pw (make-parameter #f))
(define platform "IPHONE")
(define playback-scenario "HTTP_CLOUD_TABLET_60")


(define (get-session-key game-pk event-id media-playback-id)
  (define session-header (list "Accept: application/json"
                               "Accept-Encoding: identity"
                               "Accept-Language: en-US,en;q=0.8"
                               "Connection: keep-alive"
                               (~a "User-Agent: " (current-user-agent))
                               (~a "Authorization: " (current-auth-token))
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

(define (build-string-url game-pk event-id media-playback-id)
  (define stream-header (list "Accept: */*"
                              "Accept-Encoding: identity"
                              "Accept-Language: en-US,en;q=0.8"
                              "Connection: keep-alive"
                               (~a "User-Agent: " (current-user-agent))
                               (~a "Authorization: " (current-auth-token))
                               "Proxy-Connection: keep-alive"))

  (define stream-url (url "https" #f "mf.svc.nhl.com" #f #t
                          (map (λ (path) (path/param path '())) '("ws" "media" "mf" "v.24" "stream"))
                          `((contentId . ,media-playback-id)
                            (playbackScenario . ,playback-scenario)
                            (platform . ,platform)
                            (sessionKey . ,(form-urlencoded-encode (get-session-key game-pk event-id media-playback-id)))
                            (cdnName . "MED2_AKAMAI_SECURE"))
                          #f))

  (call/input-url stream-url get-pure-port (compose string->jsexpr port->string) stream-header))

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

(define (scheduled-games->slack-response schedule)
  (define attachment-list
    (map (λ (game)
           (let* ((home (hash-ref (hash-ref game 'teams) 'home))
                  (away (hash-ref (hash-ref game 'teams) 'away))
                  (home-record (hash-ref home 'leagueRecord))
                  (away-record (hash-ref away 'leagueRecord))
                  (home-team (hash-ref home 'team))
                  (away-team (hash-ref away 'team))
                  (home-short-str (hash-ref home-team 'shortName))
                  (away-short-str (hash-ref away-team 'shortName))
                  (home-record-str (~a (hash-ref home-record 'wins)
                                       (hash-ref home-record 'losses)
                                       (hash-ref home-record 'ot) #:separator "-"))
                  (away-record-str (~a (hash-ref away-record 'wins)
                                       (hash-ref away-record 'losses)
                                       (hash-ref away-record 'ot) #:separator "-")))
             (make-hash `((markdwn_in . ("text"))
                          (fields . (
                                     #hash((title . ,home-short-str)
                                           (value . ,home-record-str)
                                           (short . #t))
                                          #hash((title . ,away-short-str)
                                                (value . ,away-record-str)
                                                (short . #t))))))))

           (hash-ref (first (hash-ref schedule 'dates)) 'games)))

  (make-hash `((username . "NHL Bot")
               (text . "Today's Games")
               (attachments . ,attachment-list))))

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

    (values (current-auth-token) auth-response-header auth-response)))

(define (fetch-teams-data)
  (define teams-url (url "http" #f "statsapi.web.nhl.com" #f #t
                         (map (λ (path) (path/param path '())) '("api" "v1" "teams")) '() #f))

  (define teams-header (list "Accept: */*"
                             "Accept-Encoding: gzip, deflate"
                             "Accept-Language: en-US,en;q=0.8"
                             (~a "User-Agent: " (current-user-agent))))

  (define teams-response (call/input-url teams-url
                                         get-pure-port
                                         (compose string->jsexpr port->string)))


  (map (λ (team-json)
         (team (hash-ref team-json 'id)
               (hash-ref team-json 'name)
               (hash-ref team-json 'abbreviation)
               (hash-ref team-json 'teamName)
               (hash-ref team-json 'locationName)
               (hash-ref team-json 'shortName)
               (hash-ref team-json 'officialSiteUrl)))
       (hash-ref teams-response 'teams)))
