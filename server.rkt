#lang racket

(require xml net/url)

(define board (list
              (list 1 2 3 4 5)
              (list 6 7 8 9 10)
              (list 11 12 13 14 15)
              (list 16 17 18 19 20)
              (list 21 22 23 24 25)))

(define (random-board)
   (shuffle (rest (build-list 26 values))))

;###########################################################################################
; holds game state
;###########################################################################################

(define dequeue-id (make-channel))

(define (hold id)
  (sync (handle-evt dequeue-id (lambda (reply-ch)
                                 (channel-put reply-ch id)
                                 (hold (+ id 1))))))

(thread (lambda () (hold 0)))

(define map-set (make-channel))
(define map-get (make-channel))

(define (hold-map mp)
  (sync
   (handle-evt map-set (lambda (item)
                         (hold-map (hash-set mp item (make-channel)))))
   (handle-evt map-get (lambda (reply-pair)
                                 (channel-put (cdr reply-pair) (hash-ref mp (car reply-pair)))
                                 (hold-map mp)))))

(thread (lambda () (hold-map (hash))))
;#############################################################################################

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

;##############################################################################################
;# Accept and handle all the input connections
;##############################################################################################
(define (accept-and-handle listener) 
    (define-values (in out) (tcp-accept listener))
   (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out)
              )))

;###############################################################################################
;# find respective channel for a input connection
;# based on the player id.
;###############################################################################################
(define (find-channel id)
  (define reply-ch (make-channel))
  (if (even? id)
     (channel-put map-get (cons id reply-ch))
     (channel-put map-get (cons (- id 1) reply-ch)))
  (channel-get reply-ch))

;###############################################################################################
;# Handle input connection
;###############################################################################################
(define (handle in out)
  (define data_str (read-line in))

  (define command_lst (string-split data_str))
  (define data (first command_lst)) 
  (define id (second command_lst))

  (cond
    [(equal? data "fetch") (let* ([dta (channel-get (find-channel (string->number id 10)))])
                             (displayln dta out)
                             (flush-output out))]
    [(equal? data "start") (let* ([reply-ch (make-channel)])
                             (channel-put dequeue-id reply-ch)
                             (define available_id (channel-get reply-ch))
                             (if (even? available_id)
                                 (channel-put map-set available_id)
                                 empty)
                             (displayln (cons available_id (random-board)) out)
                             (flush-output out))]
    [else (channel-put (find-channel (string->number id 10)) data)]))
