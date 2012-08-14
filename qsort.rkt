;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; quick-sort : (listof number)  ->  (listof number)
;; to create a list of numbers with the same numbers as
;; alon sorted in ascending order
(define (quick-sort alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [(< (length alon) 4) (mysort alon)]
    [else (append (quick-sort (smaller-items alon (first alon)))
                  (list (first alon))
                  (quick-sort (larger-items alon (first alon))))]))

;; sort : (List of Number) -> (listof number)
;; sorts a list of numbers.  Not efficient for larger lists.
(define (mysort alon)
  (local
    ((define (insert n alon)
      (if (empty? alon) (cons n empty)
        (cond [(>= n (first alon)) (cons n alon)]
              [(< n (first alon)) (cons (first alon)
                                        (insert n (rest alon)))]))))
  (if (empty? alon) empty
    (insert (first alon) (mysort (rest alon))))))

;; larger-items : [listof Number], Number -> [Listof Number]
;; creates a list with all those numbers in alon that are larger
;; than threshold
(define (larger-items alon threshold)
  (cond
    [(empty? alon) empty]
    [else (if (> (first alon) threshold)
              (cons (first alon) (larger-items (rest alon) threshold))
              (larger-items (rest alon) threshold))]))

;; smaller-items : [Listof Number], Number -> [Listof Number]
;; to create a list with all those numbers on alon that are smaller
;; than threshold
(define (smaller-items alon threshold)
  (cond [(empty? alon) empty]
	[else (if (< (first alon) threshold)
		(cons (first alon) (smaller-items (rest alon) threshold))
		(smaller-items (rest alon) threshold))]))

(quick-sort '(5 8 7 9 0 6 2 4 3 1))
