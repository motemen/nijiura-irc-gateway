#!gosh
(use srfi-1)
(use srfi-19)
(use util.list)
(use rfc.http)
(use rfc.pseudo-irc-server)
(use gauche.threads)
(use www.futaba)

(define *watching-boards*  '()) ; ((board-url . last-updated) ...)
(define *watching-threads* '()) ; ((thread-url . last-updated) ...)

(make <pseudo-irc-server>)

(irc-server-register-default-callbacks)

(on-command JOIN (client message)
  (let1 channel (car (ref message 'params))
    (rxmatch-cond
      ((#/\#(\w+)/ channel)
         (#f server)
       (start-watching-board #`"http://,|server|.2chan.net/b/"))
      ((#/\#(\w+)\/(\d+)/ channel)
         (#f server no)
       (start-watching-board #`"http://,|server|.2chan.net/b/res/,|no|.htm")))))

(on-command REFRESH (client message)
  (refresh-boards))

(define (start-watching-board url)
  (push! *watching-boards* (cons url #f)))

(define (stop-watching-board url)
  (delete! url *watching-boards*))

(define (start-watching-thread url)
  (push! *watching-threads* (cons url #f)))

(define (stop-watching-thread url)
  (delete! url *watching-threads*))

(define (refresh-boards)
  (for-each
    (lambda (pair)
      (receive (url last-updated) (car+cdr pair)
        (let1 threads (futaba-parse-index (url->string url))
          (for-each
            (lambda (thread)
              (when (or (not last-updated)
                        (date<? last-updated (assoc-ref thread 'date)))
                (irc-send-message-to
                  (url->channel url)
                  #f
                  'NOTICE
                  (url->channel url)
                  #`",(assoc-ref thread 'path) ,(assoc-ref thread 'body)")))
            threads)))
      (set-cdr! pair (current-date)))
    *watching-boards*))

(define (refresh-threads)
  (for-each
    (lambda (url)
      (for-each
        print
        (filter (lambda (res) (date<? (current-date) (assoc-ref res 'date))) (futaba-thread-url->list url))
        ))
    *watching-threads*))

(define (date<? date0 date1)
  (time<? (date->time-utc date0) (date->time-utc date1)))

(define (url->channel url)
  (rxmatch-cond
    ((#/http:\/\/(\w+)\.2chan.net\/b\// url)
       (#f server)
     #`"#,server")
    ((#/http:\/\/(\w+)\.2chan.net\/b\/res\/(\d+)/ url)
       (#f server no)
     #`"#,|server|/,|no|")))

(irc-server-start)
