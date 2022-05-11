#lang racket

;;; Wrapper around 'postgresql-connect' that takes the 
;;; parameters from a database url.

(provide pg-connect)

(require db)

(define (parse-url url)
  (define (skip-postgres url) (substring url 11))
  (define (parse-until char chars)
    (define (pred curr) (not (char=? curr char)))
    (define-values (lst rest) (splitf-at chars pred))
    (values (list->string lst) (cdr rest)))

  (define chars (string->list (skip-postgres url)))
  (let*-values ([(user rest) (parse-until #\: chars)]
                [(password rest) (parse-until #\@ rest)]
                [(server rest) (parse-until #\: rest)]
                [(_ rest) (parse-until #\/ rest)])
    (hasheq 'user user
            'password password
            'server server
            'database (list->string rest))))

(define (pg-connect url)
  (define params (parse-url url))
  (define (ref param) (hash-ref params param))
  (postgresql-connect #:user (ref 'user)
                      #:database (ref 'database)
                      #:server (ref 'server)
                      #:ssl 'yes
                      #:password (ref 'password)))

(module+ test
  (require rackunit)
  (test-case "Test parse-url"
    (define database-url "postgres://user:password@server:port/database")  
    (define expected-results (list "user" "password" "server" "database"))
    (define table (parse-url database-url))
    (define (check-result expected-result)
      (define actual-result 
        (hash-ref table (string->symbol expected-result)))
      (check-equal? expected-result actual-result))
    (for-each check-result expected-results)))
