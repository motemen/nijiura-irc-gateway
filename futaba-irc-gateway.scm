#!/usr/local/bin/gosh

(use rfc.http)
(use rfc.pseudo-irc-server)
(load "htmlprag.scm")

(define *server* (make <pseudo-irc-server>))

(irc-server-register-default-callbacks *server*)

(irc-server-start *server*)
