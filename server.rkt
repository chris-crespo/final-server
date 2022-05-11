#lang racket

(require web-server/http
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/servlet-dispatch
         web-server/web-server
         web-server/http/json
         json)

(require db)

(define (parse-url url)
  (define (skip-postgres url) (substring url 11))
  (define (parse-until char chars)
    (define (pred curr) (not (char=? curr char)))
    (define-values (lst rest) (splitf-at chars pred))
    (values (list->string lst) (cdr rest)))
  (let ([chars (string->list (skip-postgres url))])
    (let*-values ([(user rest) (parse-until #\: chars)]
                  [(password rest) (parse-until #\@ rest)]
                  [(server rest) (parse-until #\: rest)]
                  [(_ rest) (parse-until #\/ rest)])
      (hasheq 'user user
              'password password
              'server server
              'database (list->string rest)))))

(define params (parse-url (getenv "DATABASE_URL")))
(define pgc
  (postgresql-connect #:user (hash-ref params 'user)
                      #:database (hash-ref params 'database)
                      #:server (hash-ref params 'server)
                      #:ssl 'yes
                      #:password (hash-ref params 'password)))

;; Negates a predicate
(define ((neg pred) . args)
  (not (apply pred arg)))

(define ((available? column) value)
  (define query (format "select count(*) from app_user where ~a = $1" column))
  (zero? (query-value pgc query value)))

(define username-available? (available? "username"))
(define email-available? (available? "email"))

(define username-exists? (not username-available?))
(define email-exists? (not email-exists?))

(define (api/available req)
  ;; TODO: use request variables
  (response/jsexpr
    (hasheq 'username (username-available? "croissant")
            'email (email-available? "croissant"))))

(define (api/register req)
  ;; TODO: register user in database using request variables (json or url?)
  ;; TODO: should we handle errors that could arise from inserting duplicated
  ;; data? 
  (response/jsexpr))


(define-values (app reverse-uri)
  (dispatch-rules
    [("api" "available") api/available]
    [("api" "register") api/register]
    ))

(define (not-found req)
  (response/jsexpr
    (string->jsexpr "{\"error\": \"not found\"}")))

(define stop
  (serve 
    #:dispatch (sequencer:make
                 (dispatch/servlet app)
                 (dispatch/servlet not-found))
    #:port 8000))

(with-handlers ([exn:break? (lambda (e) (stop))])
  (sync/enable-break never-evt))
