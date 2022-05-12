#lang racket

(require web-server/http
         web-server/http/bindings
         web-server/http/json
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/servlet-dispatch
         web-server/web-server
         net/url
         db 
         json 
         file/sha1) ; Used to converted the result of sha256-bytes back into a string 

(define db-url (string->url (getenv "DATABASE_URL")))
(match-define (url _ username/password host _ _ (list (path/param database _)) _ _)
  db-url)
(match-define (list username password) (string-split username/password ":"))
(define pgc
  (postgresql-connect #:user username 
                      #:password password  
                      #:server host
                      #:database database
                      #:ssl 'yes))

(define ((available? column) value)
  (define query (format "select count(*) from app_user where ~a = $1" column))
  (zero? (query-value pgc query value)))

(define username-available? (available? "username"))
(define email-available? (available? "email"))

(define username-exists? (negate username-available?))
(define email-exists? (negate email-available?))

;;; Macros to handle request params and post data
(define-syntax-rule (with-request-params request (param ...) expr0 . exprs)
  (begin
    (define bindings (request-bindings/raw request))
    (let ((param (bind-id (symbol->bytes (quote param)) bindings)) ...) 
      expr0 . exprs)))

(define-syntax-rule (with-post-data request (param ...) expr0 . exprs)
  (begin
    (define data (bytes->jsexpr (request-post-data/raw request)))
    (let ((param (hash-ref data (quote param) #f)) ...)
      expr0 . exprs)))

;;; Request parameter binding utilities
(define symbol->bytes (compose string->bytes/utf-8 symbol->string))

;; Converts arg to sql-null if it's #f
(define/match (->sql-null arg)
  [(#f) sql-null]
  [(_) arg])

(define (bind-id id bindings)
  (match
    (bindings-assq id bindings)
    [(? binding:form? b)
     (bytes->string/utf-8 (binding:form-value b))]
    [_ #f]))

(define (bind-list keys bindings)
  (define (bind key) (bind-id key bindings))
  (map bind keys))

;;; Error handlers
(define (handlers/register e)
  (define info (exn:fail:sql-info e))
  (define message (cdr (assoc 'message info)))
  (hasheq 'message message))

;;; Api routes
(define (api/user req)
  (with-request-params req (user)
    (define res
      (query-maybe-row
        pgc #<<SQL
select username, email, concat(first_name, ' ', last_name), phone 
from app_user 
where username = $1 or email = $1
SQL
        user))
        (println res)
    (response/jsexpr
      (match res
        [(vector username email name phone)
         (hasheq 'username username
                 'email email
                 'name name
                 'phone phone)]
        [_ (hasheq 'message "User not found")]))))

(define (api/user/auth req)
  (with-request-params req (user password)
    (define user-password 
      (query-maybe-value 
        pgc "select password from app_user where username = $1 or email = $1"
        user))
    (define (valid-password? user-password) 
      ;; TODO: not passing password as parameter returns sql-null
      (string=? password user-password))
    (response/jsexpr
      (if password
          (hasheq 'user #t 'password (valid-password? user-password))
          (hasheq 'user #f 'pasword #f))
      #:headers (list (header #"Access-Control-Allow-Origin" #"*")))))

(define (api/user/available req)
  ;; The response is a json object with information about
  ;; both the email and username availability. It always
  ;; returns both independent of the request parameters,
  ;; which works fine for the way the web client uses the
  ;; api, but a better approach would be to just return
  ;; the availability for the given request parameters.
  (with-request-params req (username email)
    (define username? (username-available? (->sql-null username)))
    (define email? (email-available? (->sql-null email)))
    (response/jsexpr
      (hasheq 'username (and username username?)
              'email (and email email?)))))

(define (api/user/register req)
  (define (insert-user . args)
    (apply
      query pgc "insert into app_user values ($1, $2, $3, $4, $5, $6)"
      (map ->sql-null args)))
  (with-post-data req (username email password first-name last-name phone)
    (response/jsexpr
      (with-handlers ([exn:fail:sql? handlers/register])
        (insert-user 
          username 
          email 
          (bytes->hex-string (sha256-bytes (string->bytes/utf-8 password))) 
          first-name 
          last-name 
          phone) 
        (hasheq 'success #t)))))

(define-values (app reverse-uri)
  (dispatch-rules
    [("api" "user") api/user]
    [("api" "user" "auth") api/user/auth]
    [("api" "user" "available") api/user/available]
    [("api" "user" "register") #:method "post" api/user/register]))

(define (not-found req)
  (response/jsexpr
    (string->jsexpr "{\"error\": \"not found\"}")))

(define port 
  (if (getenv "PORT")
      (string->number (getenv "PORT")) 
      8000))

(define stop
  (serve 
    #:dispatch (sequencer:make
                 (dispatch/servlet app)
                 (dispatch/servlet not-found))
    #:listen-ip #f
    #:port port))

(with-handlers ([exn:break? (lambda (e) (stop))])
  (sync/enable-break never-evt))
