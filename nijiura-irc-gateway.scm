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
(use www.futaba.nijiura)

(define *watching-urls* '())                 ; ((url . last-updated) ...)
(define *cache* (make-hash-table 'string=?)) ; url => responses or threads

;;;; Utility
(define-syntax fork-do
  (syntax-rules ()
    ((_ body ...)
     (thread-start!
       (make-thread
         (lambda () body ...))))))

(define-syntax define-asynchronous-proc
  (syntax-rules ()
    ((_ (formals ...) body ...)
     (define (formals ...)
       (thread-start!
         (make-thread
           (lambda () body ...)))))))

(define (date<? date0 date1)
  (time<? (date->time-utc date0) (date->time-utc date1)))

(define (for-each-line proc string)
  (for-each proc (string-split string #[\r\n])))

(define (log-message message)
  (print message)
  (irc-send-message-to '* #f 'NOTICE message))

;;;; Main code
;;; Fetching
(define (nijiura-get/cache url)
  (receive (items url-type status) (nijiura-get url)
    (log-message #`",status ,url")
    (hash-table-update!
      *cache*
      url
      (pa$ lset-union
           (lambda (x y)
             (equal? (assoc-ref x 'no) (assoc-ref y 'no)))
           items)
      '())
    (values items url-type status)))

;;; Watching
(define (start-watching url)
  (log-message #`"start watching ,url")
  (push! *watching-urls* (cons url (time-utc->date (make-time 'time-utc 0 0))))
  (show-backlog url))

(define (stop-watching url)
  (log-message #`"stop watching ,url")
  (update! *watching-urls* (pa$ remove! (lambda (p) (string=? (car p) url)))))

;;; Show log
(define (send-item type channel nick item . options)
  (let-keywords options ((prefix #f))
    (for-each-line
      (lambda (line)
        ((if (eq? type 'privmsg) irc-privmsg-to irc-notice-to)
         channel
         nick
         (if prefix #`",prefix ,line" line)))
      (assoc-ref item 'body))))

(define-asynchronous-proc (refresh)
  (dolist (url+last-updated *watching-urls*)
    (let*-values (((url last-updated) (car+cdr url+last-updated))
                  ((items url-type status) (nijiura-get/cache url)))
      (dolist (item (or items '()))
        (when (or (not (eq? url-type 'thread)) (date<? last-updated (assoc-ref item 'date)))
          (irc-notice-to (url->channel url) "-" " ")
          (apply
            send-item
            'privmsg
            (case url-type
              ((index)
               (list (url->channel url)
                     (url+path->channel url (assoc-ref item 'path))
                     item))
              ((thread)
               (list (url->channel url)
                     (string-join `(,#`"No.,(assoc-ref item 'no)" ,@(cond ((assoc-ref item 'mail) => (lambda (m) (list #`"[,m]"))) (else '()))))
                     item)))))))
    (set-cdr! url+last-updated (current-date))))

(define-asynchronous-proc (show-backlog url)
  (receive (items url-type status) (nijiura-get/cache url)
    (dolist (item (or items '()))
      (irc-notice-to (url->channel url) "-" " ")
      (apply
        send-item
        'notice
        (case url-type
          ((index)
           (list (url->channel url)
                 (url+path->channel url (assoc-ref item 'path))
                 item
                 :prefix (date->string (assoc-ref item 'date) "~X")))
          ((thread)
           (list (url->channel url)
                 (string-join `(,#`"No.,(assoc-ref item 'no)" ,@(cond ((assoc-ref item 'mail) => (lambda (m) (list #`"[,m]"))) (else '()))))
                 item
                 :prefix (date->string (assoc-ref item 'date) "~X")))))))
  (set-cdr! (assoc url *watching-urls*) (current-date)))

;;; PRIVMSG commands
(define (command-grep channel word)
  (for-each
    (lambda (item)
      (when (string-contains (assoc-ref item 'body) word)
        (send-item
          irc-privmsg-to
          channel
          "grep"
          item)))
    (hash-table-get *cache* (channel->url channel) '())))

;;; URL <-> Channel
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

;;; Main
(define (main args)
  (make <pseudo-irc-server> :name "nijiura")

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
         #f
         (channel->url channel)))))

  (irc-on-command REFRESH ()
    (refresh))

  (irc-on-command EVAL (client . params)
    (let1 result (guard (e (else e))
                   (eval (call-with-input-string (string-join params) read) (current-module)))
      (irc-notice-to client #f (x->string result))))

  (fork-do
    (while #t
      (refresh)
      (thread-sleep! 120)))

  (irc-server-start))
