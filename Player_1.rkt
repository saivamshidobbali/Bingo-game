#lang racket
(require "helper.rkt")

(define (play board client_id game_over)
  (if (bingo? board game_over)
      (handle-termination board client_id)
      (let* ([move (display-and-make-move board client_id)]
             [game_end (car move)]
             [new_board (cdr move)])
        (if (equal? game_end #t)
            (displayln "You Won")
            (let* ([fetch-result (display-and-fetch-move new_board client_id)])
            (play (cdr fetch-result) client_id (car fetch-result)))))))

(define game_info (initiate-game))
(play (car game_info) (cdr game_info) #f)