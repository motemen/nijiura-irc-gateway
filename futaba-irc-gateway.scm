#!gosh
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

(define *watching-urls* '()) ; ((url . last-updated) ...)

(define-syntax fork-do
  (syntax-rules ()
    ((_ body ...)
     (thread-start!
       (make-thread
         (lambda () body ...))))))

(define (date<? date0 date1)
  (time<? (date->time-utc date0) (date->time-utc date1)))

(define (for-each-line proc string)
  (for-each proc (string-split string #[\r\n])))

(define (log-message message)
  (print message)
  (irc-send-message-to '* #f 'NOTICE message))

(define (start-watching url)
  (log-message #`"start watching ,url")
  (push! *watching-urls* (cons url (time-utc->date (make-time 'time-utc 0 0))))
  (show-backlog url))

(define (stop-watching url)
  (log-message #`"stop watching ,url")
  (update! *watching-urls* (pa$ remove! (lambda (p) (string=? (car p) url)))))

(define (refresh)
  (fork-do
    (dolist (url+last-updated *watching-urls*)
      (let*-values (((url last-updated) (car+cdr url+last-updated))
                    ((items url-type status) (futaba-url->list url)))
        (log-message #`",status ,url")
        (dolist (item (or items '()))
          (when (or (not (eq? url-type 'thread)) (date<? last-updated (assoc-ref item 'date)))
            (for-each-line
              (lambda (line)
                (case url-type
                  ((index) 
                   (irc-privmsg-to
                     (url->channel url)
                     (url->channel url)
                     #`",(url+path->channel url (assoc-ref item 'path)) ,line"))
                  ((thread)
                   (irc-privmsg-to
                     (url->channel url)
                     (no->ident (assoc-ref item 'no))
                     line))))
              (assoc-ref item 'body))
            (irc-notice-to (url->channel url) "-" " "))))
      (set-cdr! url+last-updated (current-date)))))

(define (show-backlog url)
  (fork-do
    (receive (items url-type status) (futaba-url->list url)
      (log-message #`",status ,url")
      (dolist (item (or items '()))
        (for-each-line
          (lambda (line)
            (case url-type
              ((index) 
               (irc-notice-to
                 (url->channel url)
                 (url->channel url)
                 #`"(,(date->string (assoc-ref item 'date) \"~X\")) ,(url+path->channel url (assoc-ref item 'path)) ,line"))
              ((thread)
               (irc-notice-to
                 (url->channel url)
                 (no->ident (assoc-ref item 'no))
                 #`"(,(date->string (assoc-ref item 'date) \"~X\")) ,line"))))
          (assoc-ref item 'body))
        (irc-notice-to (url->channel url) "-" " ")))
    (set-cdr! (assoc url *watching-urls*) (current-date))))

(define (show-thread-backlog url)
  (let1 responses (or (futaba-url->list url) '())
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

  (irc-on-command PRIVMSG (client channel msg)
    (cond
      ((string=? msg "url")
       (irc-notice-to
         channel
         (irc-prefix-of (current-irc-server)) ; TODO #f または (current-irc-server) とできるとよい
         (channel->url channel)))))

  (irc-on-command REFRESH ()
    (refresh))

  (irc-on-command EVAL (client . params)
    (let1 result (guard (e (else e))
                   (eval (call-with-input-string (string-join params) read) (current-module)))
      (irc-send-notice-to-client (current-irc-server) client result)))

  (fork-do
    (while #t
      (refresh)
      (thread-sleep! 120)))

  (irc-server-start))

(main '())
