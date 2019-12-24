;;;----------------------------------------------------------------------
;;; Multi-Dimensional Arrays
;;; FILE: multi-dim-arrays.scm
;;; AUTHOR: Alfred J. Reich
;;; VERSION: 1.0
;;; LANGUAGE: Scheme
;;; DESCRIPTION: A multi-dimensional array (MDA) is a vector of
;;; vectors, of vectors, etc.  This code provides procedures for
;;; converting MDAs to and from lists, retrieving and setting
;;; individual MDA elements, and obtaining the dimensions of an MDA.
;;; (I use MDAs to represent probability tables in Bayesian networks.)
;;;----------------------------------------------------------------------
;;; This software is provided as is, with no warranty, either express
;;; or implied.  In no event shall the author be liable for
;;; consequential or incidental damages of any nature whatsoever.  You
;;; may copy this software, in whole or in part, and use it for any
;;; lawful purpose.
;;;----------------------------------------------------------------------
;;; PROCEDURES:
;;; (array-dimensions array)		; ==> list of numbers
;;; (array-ref array . indices)		; ==> a Scheme object
;;; (array-set! array value . indices)	; ==> <undefined>
;;; (list->array list)			; ==> an array
;;; (array->list array)			; ==> a list
;;;
;;; EXAMPLES:
;;;  See end of this file.
;;;----------------------------------------------------------------------

;;; Returns an ordered list containing the dimensions of the input
;;; array.
(define (array-dimensions array)
  (let loop ((arr array) (dims '()))
    (if (vector? arr)
	(loop (vector-ref arr 0) (cons (vector-length arr) dims))
	(reverse dims))))

;;; Returns the array element at the array location indicated by the
;;; ordered list of indices.  Indices is not optional and must be a
;;; 0-based list of numbers whose length equals the number of
;;; dimensions in the array.
(define (array-ref array . indices)
  (let loop ((arr array) (ind indices))
    (if (and (vector? arr) (not (null? ind)))
	(loop (vector-ref arr (car ind)) (cdr ind))
	arr)))

;;; Sets the array element to a value at the array location indicated
;;; by the ordered list of indices.  Indices have the same constraints
;;; here as in array-ref, above.
(define (array-set! array value . indices)
  (let loop ((arr array) (ind indices))
    (if (and (vector? arr) (not (null? ind)))
	(if (= (length ind) 1)
	    (vector-set! arr (car ind) value)
	    (loop (vector-ref arr (car ind)) (cdr ind)))
	(display 'error))))

;;; Turns a list (of lists, of lists,...) into an array (a vector of
;;; vectors of...).  By the way, this is one time when a non-tail
;;; recursive procedure is handy.  list->array is modeled after the
;;; naive version of 'factorial' found in most introductory Scheme
;;; texts.
(define (list->array list)
  (if (not (pair? (car list)))
      (list->vector list)
      (list->vector (map list->array list))))

;;; Turns an array into a list.  The complement to list->array, above.
;;; Requires the procedure "vector-map" (see Scheme Utilities at my
;;; web page).
(define (array->list array)
  (if (zero? (vector-length array))
      '()
      (if (not (vector? (vector-ref array 0)))
	  (vector->list array)
	  (vector->list (vector-map array->list array)))))

;;;----------------------------------------------------------------------
;;; Examples

(define arr1 '#(
		#(
		  #(0.08 0.16 0.08)
		  #(0.05 0.05 0.05)
		  #(0.05 0.05 0.05)
		  )
		#(
		  #(0.02 0.04 0.02)
		  #(0.00 0.00 0.00)
		  #(0.10 0.10 0.10)
		  )))

;;; (array->list arr1)
;;; => (((.08 .16 .08)
;;;      (.05 .05 .05)
;;;      (.05 .05 .05))
;;;     ((.02 .04 .02)
;;;      (0. 0. 0.)
;;;      (.1 .1 .1)))

(define lst1 '((
		(0.08 0.16 0.08)
		(0.05 0.05 0.05)
		(0.05 0.05 0.05)
		)
	       (
		(0.02 0.04 0.02)
		(0.00 0.00 0.00)
		(0.10 0.10 0.10)
		)
	       ))

;;; (list->array lst1)
;;; ==> #(#(#(.08 .16 .08)
;;;         #(.05 .05 .05)
;;;         #(.05 .05 .05))
;;;       #(#(.02 .04 .02)
;;;         #(0. 0. 0.)
;;;         #(.1 .1 .1)))

;(array-dimensions arr1)		; ==> (2 3 3)
;(array-ref arr1 0 0 1)			; ==> .16
;(array-ref arr1 1 0 1)			; ==> .04

;(pretty-print arr1)
; ==> #(#(#(.08 .16 .08) #(.05 .05 .05) #(.05 .05 .05))
;       #(#(.02 .04 .02) #(0. 0. 0.) #(.1 .1 .1)))
;(array-set! arr1 .007 0 0 1)
;(pretty-print arr1)
; ==> #(#(#(.08 .007 .08) #(.05 .05 .05) #(.05 .05 .05))
;       #(#(.02 .04 .02) #(0. 0. 0.) #(.1 .1 .1)))

;;;----------------------------------------------------------------------
;;; END OF FILE
