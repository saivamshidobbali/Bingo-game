#lang racket

(define server-address "localhost")
(define port-no 8081)

(define (display-board board)
  (for ([i (in-range 5)])
    (for ([j (in-range 5)])
      (printf "~a\t" (list-ref (list-ref board i) j)))
    (newline)))

(define (change-at strike-off lst)
  (for/list ([i '(0 1 2 3 4)])
    (if (equal? (list-ref lst i) strike-off) "X" (list-ref lst i))))

(define (crossed? elem)
  (equal? elem "X"))

(define (Istrue? elem)
  (equal? #t elem))

(define (check_rows board)
  (count Istrue? (for/list ([i (in-range 5)])
                   (= (count crossed? (list-ref board i)) 5))))

(define (check_cols board)
  (count Istrue?
  (for/list ([j (in-range 5)])
       (or (or (or (or
            (list-ref (list-ref board 0) j)
            (list-ref (list-ref board 1) j))
            (list-ref (list-ref board 2) j))
            (list-ref (list-ref board 3) j))
            (list-ref (list-ref board 4) j)))))

(define (boolean-to-number num)
  (cond
   [(equal? num #t) 1]
   [(equal? num #f) 0]))

(define (check_diagonals board)

(+

(boolean-to-number (and (and (and (and
  (equal? (list-ref (list-ref board 0) 4) "X")
  (equal? (list-ref (list-ref board 1) 3) "X"))
  (equal? (list-ref (list-ref board 2) 2) "X"))
  (equal? (list-ref (list-ref board 3) 1) "X"))
  (equal? (list-ref (list-ref board 4) 0) "X")))

(boolean-to-number
 (and (and (and (and
  (equal? (list-ref (list-ref board 0) 0) "X")
  (equal? (list-ref (list-ref board 1) 1) "X"))
  (equal? (list-ref (list-ref board 2) 2) "X"))
  (equal? (list-ref (list-ref board 3) 3) "X"))
  (equal? (list-ref (list-ref board 4) 4) "X")))

))

(define (bingo? board game_over)
  (if (equal? game_over #t)
      #t
    (> (+ (+ (check_rows board) (check_cols board)) (check_diagonals board)) 3)))

(define (update-board board move)
  (map (lambda(lst) (if (equal? (member move lst) #f) lst (change-at move lst))) board))



;##################################################################################
; initiate the game
;##################################################################################
(define (initiate-game)
  (define-values (in out) (tcp-connect server-address port-no))
  (display "start game" out)
  (newline out)
  (flush-output out)
  (define board_n_id (read-line in))
  (close-input-port in)
  (close-output-port out)

  (define client_id (first (string-split (substring board_n_id 1 (- (string-length board_n_id) 1)))))
  (define b (rest (string-split (substring board_n_id 1 (- (string-length board_n_id) 1)))))

  (define board
    (list
     (list (list-ref b 0) (list-ref b 1) (list-ref b 2) (list-ref b 3) (list-ref b 4))
     (list (list-ref b 5) (list-ref b 6) (list-ref b 7) (list-ref b 8) (list-ref b 9))
     (list (list-ref b 10) (list-ref b 11) (list-ref b 12) (list-ref b 13) (list-ref b 14))
     (list (list-ref b 15) (list-ref b 16) (list-ref b 17) (list-ref b 18) (list-ref b 19))
     (list (list-ref b 20) (list-ref b 21) (list-ref b 22) (list-ref b 23) (list-ref b 24))))

  (cons board client_id))
;#######################################################################################

(define (display-and-fetch-move board client_id)
  (define op-move (fetch-opponent-move client_id))
  (define updated_board (update-board board op-move))
  (displayln "Opponent's move")
  (display-board updated_board)
  (display "\n\n")
  (if (or (equal? op-move "-1") (bingo? updated_board #f))
      (cons #t updated_board)
      (cons #f updated_board)))


(define (display-and-make-move board client_id)
  (define new_board (make-move board client_id)) 
  (display-board new_board)
  (display "\n\n")
  (if (bingo? new_board #f)
      (cons #t new_board)
      (cons #f new_board)))

(define (handle-termination board client_id)
  (displayln "You Won")
  (define-values (cin cout) (tcp-connect server-address port-no))
  (display (string-append (string-append "-1" " ") client_id) cout) 
  
  (newline cout)
  (flush-output cout)
  (close-input-port cin)
  (close-output-port cout))

(define (make-move board client_id)
  ; Connect with server and register your move
  (displayln "Enter number of your choice:")  
  (define num (read-line))
  
  (define new_board (update-board board (string-trim num)))
  (define-values (cin cout) (tcp-connect server-address port-no))
  
  (if (bingo? new_board #f)  
      (display (string-append (string-append "-1" " ") client_id) cout)
      (display (string-append (string-append num " ") client_id) cout))
  
  (newline cout)
  (flush-output cout)
  (close-input-port cin)
  (close-output-port cout)
  new_board)

(define (fetch-opponent-move client_id)
  ; Connect with the server and fetch opponents move
  (define-values (in out) (tcp-connect server-address port-no))

  (display (string-append (string-append "fetch ") client_id) out)
  (newline out)
  (flush-output out)
  (define move (read-line in))

  (close-input-port in)
  (close-output-port out)
  move)

(provide (all-defined-out))