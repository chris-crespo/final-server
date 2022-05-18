#lang racket

(require web-server/http
         web-server/http/bindings
         web-server/http/json
         web-server/http/response-structs
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/servlet-dispatch
         web-server/web-server
         net/url
         db 
         db/util/postgresql
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
(define (sql-date->string date)
  (match-define (struct sql-date (year month day)) date)
  (format "~a-~a-~a" year month day))

(define symbol->bytes (compose string->bytes/utf-8 symbol->string))

;; Cors wrapper, influenced by koyo/cors
(define allow-origin-header (header #"Access-Control-Allow-Origin" #"*"))
(define (make-options-headers)
  (list allow-origin-header
        (header #"Access-Control-Allow-Methods" #"POST, OPTIONS")
        (header #"Access-Control-Allow-Headers" #"*")))
(define ((wrap-cors handler) req . args)
  (cond 
    [(bytes=? (request-method req) #"OPTIONS")
     (response/full 200 #"OK" (current-seconds) #f (make-options-headers) null)]
    [else
      (define resp (apply handler req args))
      (define headers (cons (header #"Access-Control-Allow-Origin" #"*")
                            (response-headers resp)))
      (struct-copy response resp [headers headers])]))

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

;; Hash string with sha256
(define sha256 
  (compose bytes->hex-string
           sha256-bytes
           string->bytes/utf-8))

;;; Error handlers
(define (handlers/register e)
  (define info (exn:fail:sql-info e))
  (define message (cdr (assoc 'message info)))
  (hasheq 'message message))

;;; Api routes
(define (api/activities req camp-id)
  (define activities
    (query-list pgc "select activity from camp_activity where camp = $1" camp-id))
  (response/jsexpr activities))

(define (api/bookings req user-email)
  (define bookings 
    (query-rows pgc #<<SQL
select k.dni as kid_dni, concat(k.first_name, ' ', k.last_name) as kid_name, camp_name
from booking b join kid k
on b.kid = k.dni
join camp c
on b.camp = c.id
where user_email = $1
SQL
  user-email))
  (define (row->booking row)
    (make-hasheq (map cons '(kid-dni kid-name camp-name) row)))
  (response/jsexpr
    (hasheq 
      'bookings (map row->booking bookings))))

(define (api/camp req id)
  (match-define (vector camp-id name kind description location start end min-age max-age langs)
    (query-row pgc #<<SQL
select camp.*, array_agg(camp_lang.lang) 
from camp join camp_lang
on camp.id = camp_lang.camp
where id = $1
group by camp.id
SQL
  id))
  (response/jsexpr
    (hasheq
      'id camp-id
      'name name
      'kind kind
      'description description
      'location location
      'start (sql-date->string start)
      'end (sql-date->string end)
      'minAge min-age
      'maxAge max-age
      'languages (pg-array->list langs))))

(define (api/camps/kinds req)
  (define kinds 
    (query-list pgc "select kind from camp_kind"))
  (response/jsexpr
    (hasheq 'kinds kinds)))

(define (api/camps/langs req)
  (define langs
    (query-list pgc "select distinct lang from camp_lang"))
  (response/jsexpr
    (hasheq 'langs langs)))

(define (api/camps req)
  (define (row->camp row) 
    (match-define 
      (vector id camp kind description location start end min-age max-age langs)
      row)
    `(camp
       (id ,(number->string id))
       (name ,camp)
       (kind ,kind)
       (description ,description)
       (location ,location)
       (start ,(sql-date->string start))
       (end ,(sql-date->string end))
       (min-age ,(number->string min-age))
       (max-age ,(number->string max-age))
       (langs ,@(map (λ (lang) `(lang ,lang)) (pg-array->list langs)))))
    
  (define camps 
    (query-rows pgc #<<SQL
select camp.*, array_agg(camp_lang.lang) 
from camp join camp_lang 
on camp.id = camp_lang.camp
group by camp.id
SQL
  ))
  (response/xexpr
    `(camps ,@(map row->camp camps))))

(define (api/kid req user-email)
  (define (insert-kid . args)
    (apply
      query pgc "insert into kid values ($1, $2, $3, $4, $5)"
      (map ->sql-null args)))
   (with-post-data req (dni firstName lastName age)
    (response/jsexpr
      (with-handlers ([exn:fail:sql? handlers/register])
        (insert-kid dni firstName lastName age user-email) 
        (hasheq 'success #t)))))

(define (api/kids req user-email)
  (define (row->kid row) 
    (match-define (vector dni first-name last-name age user)
      row)
    (hasheq 'dni dni
            'first-name first-name
            'last-name last-name
            'age age
            'user user))
  (define kids
    (query-rows pgc "select * from kid where app_user = $1" user-email))
  (response/jsexpr (map row->kid kids)))

(define (api/user req)
  (with-request-params req (user)
    (define res
      (query-maybe-row
        pgc #<<SQL
select username, email, concat(first_name, ' ', last_name) as fullname, phone 
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
      (string=? (sha256 password) user-password))
    (response/jsexpr
      (if user-password
          (hasheq 'user #t 'password (valid-password? user-password))
          (hasheq 'user #f 'password #f)))))

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
          (sha256 password)
          first-name 
          last-name 
          phone) 
        (hasheq 'success #t)))))

(define-values (app reverse-uri)
  (dispatch-rules
    [("api" "activities" (integer-arg)) (wrap-cors api/activities)]
    [("api" "bookings" (string-arg)) (wrap-cors api/bookings)]
    [("api" "camp" (integer-arg)) (wrap-cors api/camp)]
    [("api" "camps" "kinds") (wrap-cors api/camps/kinds)]
    [("api" "camps" "langs") (wrap-cors api/camps/langs)]
    [("api" "camps") (wrap-cors api/camps)]
    [("api" "kid" (string-arg)) #:method (or "post" "options") (wrap-cors api/kid)]
    [("api" "kids" (string-arg)) (wrap-cors api/kids)]
    [("api" "user") (wrap-cors api/user)]
    [("api" "user" "auth") (wrap-cors api/user/auth)]
    [("api" "user" "available") (wrap-cors api/user/available)]
    [("api" "user" "register") #:method (or "post" "options") (wrap-cors api/user/register)]))

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
