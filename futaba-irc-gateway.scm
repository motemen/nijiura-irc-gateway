#!gosh
(define-module www.futaba.irc-gateway)
(select-module www.futaba.irc-gateway)
(use srfi-1)
(use srfi-11)
(use srfi-13)
(use srfi-19)
(use util.list)
(use rfc.http)
(use rfc.uri)
(use rfc.pseudo-irc-server)
(use gauche.charconv)
(use gauche.threads)
(use gauche.reload)
(use www.futaba)

(define *watching-boards*  '()) ; ((board-url . last-updated) ...)
(define *watching-threads* '()) ; ((thread-url . last-updated) ...)

(define-syntax fork-do
  (syntax-rules ()
    ((_ body ...)
     (thread-start!
       (make-thread
         (lambda () body ...))))))

(define (log-message message)
  (print message)
  (irc-send-message-to '* #f 'NOTICE message))

(define (start-watching url)
  (if (#/\.htm$/ url)
    (start-watching-thread url)
    (start-watching-board url)))

(define (stop-watching url)
  (if (#/\.htm$/ url)
    (stop-watching-thread url)
    (stop-watching-board url)))

(define (start-watching-board url)
  (push! *watching-boards* (cons url #f))
  (refresh-boards))

(define (stop-watching-board url)
  (delete! (lambda (p) (equal? (car p) url)) *watching-boards*))

(define (start-watching-thread url)
  (push! *watching-threads* (cons url #f))
  (guard (e (else (print e)))
    (show-thread-backlog url)))

(define (stop-watching-thread url)
  (delete! (lambda (p) (equal? (car p) url)) *watching-threads*))

(define (refresh-boards)
  (fork-do
    (dolist (pair *watching-boards*)
      (let*-values (((url last-updated) (car+cdr pair))
                    ((threads) (or (futaba-parse-index (url->string url)) '())))
        (dolist (thread threads)
          (when (or (not last-updated)
                    (date<? last-updated (assoc-ref thread 'date)))
            (for-each
              (lambda (line)
                (irc-privmsg-to
                  (url->channel url)
                  (url->channel url)
                  #`",(url+path->channel url (assoc-ref thread 'path)) ,line"))
              (string-split (assoc-ref thread 'body) #/[\r\n]/)))))
      (set-cdr! pair (current-date)))))

(define (refresh-threads)
  (fork-do
    (dolist (pair *watching-threads*)
      (let*-values (((url last-updated) (car+cdr pair))
                    ((responses) (or (futaba-parse-thread (url->string url)) '())))
        (dolist (response responses)
          (when (or (not last-updated)
                    (date<? last-updated (assoc-ref response 'date)))
            (for-each
              (lambda (line)
                (irc-privmsg-to
                  (url->channel url)
                  (no->ident (assoc-ref response 'no))
                  ;(irc-prefix-of (current-irc-server))
                  line))
              (string-split (assoc-ref response 'body) #/[\r\n]/)))))
      (set-cdr! pair (current-date)))))

(define (show-thread-backlog url)
  (let1 responses (or (futaba-parse-thread (url->string url)) '())
    (dolist (response responses)
      (for-each
        (lambda (line)
          (irc-notice-to
            (url->channel url)
            (no->ident (assoc-ref response 'no))
            ;(irc-prefix-of (current-irc-server))
            #`"(,(date->string (assoc-ref response 'date) \"~X\")) ,line"))
        (string-split (assoc-ref response 'body) #/[\r\n]/))
      ))
  (set-cdr! (assoc url *watching-threads*) (current-date)))

(define (no->ident no)
  (number->string (x->integer (string-reverse no)) 36))

(define (date<? date0 date1)
  (time<? (date->time-utc date0) (date->time-utc date1)))

(define (url->channel url)
  (rxmatch-cond
    ((#/^http:\/\/(\w+)\.2chan.net\/b\/$/ url)
       (#f server)
     #`"#,server")
    ((#/^http:\/\/(\w+)\.2chan.net\/b\/res\/(\d+).htm$/ url)
       (#f server no)
     #`"#,|server|/,|no|")))

(define (url+path->channel url path)
  (let1 channel (url->channel url)
    (rxmatch-let (#/\d+/ path) (no)
      #`",|channel|/,|no|")))

(define (channel->url channel)
  (rxmatch-cond
    ((#/^\#(\w+)$/ channel)
       (#f server)
     #`"http://,|server|.2chan.net/b/")
    ((#/^\#(\w+)\/(\d+)$/ channel)
       (#f server no)
     #`"http://,|server|.2chan.net/b/res/,|no|.htm")))

(define (url->string url)
  (receive (#f #f host #f path #f #f) (uri-parse url)
    (receive (status #f html) (http-get host path)
      (log-message #`",status ,url")
      (ces-convert html "*JP"))))

(define (main args)
  (make <pseudo-irc-server> :name "futaba")

  (irc-server-register-default-callbacks)

  (irc-on-command JOIN (client channel)
    (for-each
      (lambda (channel)
        (start-watching (channel->url channel)))
      (string-split channel ",")))

  (irc-on-command PART (client channel)
    (for-each
      (lambda (channel)
        (stop-watching (channel->url channel)))
      (string-split channel ",")))

  (irc-on-command REFRESH ()
    (refresh-boards)
    (refresh-threads))

  (irc-on-command EVAL (client . params)
    (let1 result (guard (e (else e))
                   (eval (call-with-input-string (string-join params) read) (current-module)))
      (irc-send-notice-to-client (current-irc-server) client result)))

  (fork-do
    (while #t
      (refresh-boards)
      (refresh-threads)
      (thread-sleep! 60)))

  (irc-server-start))
