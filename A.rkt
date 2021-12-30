#lang racket
(provide (all-defined-out))


(define n_items_getter
    (lambda (lst num)
        (cond [(null? lst) '()]
              [(> num 0) (cons (car lst) (n_items_getter (cdr lst) (- num 1)))]
              [#t '()]
)))



(define list_slicer_func
    (lambda (lst start count)
        (cond [(> start 1) (list_slicer_func (cdr lst) (- start 1) count)] [#t (n_items_getter lst count)]
            )))




(define (my_list_length_finder lst counter)(
cond [(null? lst) counter]
     [#t (my_list_length_finder (cdr lst) (+ counter 1))]

                             ))

(define (append lst number)(
cond [(null? lst) (cons number null)] [(null? (cdr lst)) (cons (car lst) (cons number null))]
     [#t (cons [car lst] [append (cdr lst) number] )]
                            ))




(define (indexes_list_generator length counter output_list) (
cond [(zero? (- counter length)) output_list]
     [#t (indexes_list_generator length (+ counter 1) (append output_list (+ counter 1)) )]
                                         ))

(define (list_appender first_list second_list) (
cond [(null? second_list) first_list]
[#t (list_appender (append first_list (car second_list)) (cdr second_list) ) ]
                                                ))



(define (sublists_generator lst indexes_lst output_lst start_index count) (
cond [(null? indexes_lst) output_lst]
     [(> (+ start_index count) (my_list_length_finder lst 0)) (sublists_generator lst (cdr indexes_lst) output_lst (+ start_index 1) 0)]
     [#t (sublists_generator lst indexes_lst (list_appender output_lst (list (list_slicer_func lst start_index (+ count 1) )) ) start_index (+ count 1) ) ]

                                                          ))


(define (elements_adder lst result)(
cond [(null? lst) result]
     [#t (elements_adder (cdr lst) (+ result (car lst))) ]
                             ))


(define (sublists_sum_calculator lst output_lst)(
cond [(null? lst) output_lst]
     [#t (sublists_sum_calculator (cdr lst) (append output_lst (elements_adder (car lst) 0 ) ) )]

                                      ))


(define (max_finder lst max)(
cond [(null? lst) max]
     [(> (car lst) max) (max_finder (cdr lst) (car lst))]
     [#t (max_finder (cdr lst) max)]

                         ))



(define (get_MSS ls) 
  (


   max_finder (cdr (sublists_sum_calculator (sublists_generator ls (indexes_list_generator (my_list_length_finder ls 0) 0 '() ) '() 1 0) '())) (car (sublists_sum_calculator (sublists_generator ls (indexes_list_generator (my_list_length_finder ls 0) 0 '() ) '() 1 0) '()))



   ))




































