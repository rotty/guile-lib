;;; ----------------------------------------------------------------------
;;;    minima.scm -- finding minima in mathematical functions
;;;    Copyright (C) 2003 Richard Todd
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; ----------------------------------------------------------------------
(define-module (math minima)
  #:export (golden-section-search))

(define golden-section-search
;;; Original author's notice for golden-section-search>>>>>>>>>>>>>>>>
;;; Author: Lars Arvestad
;;;
;;; This code is in the public domain.
;;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Original author's notice
  (let ((gss 'golden-section-search:)
	(r (/ (- (sqrt 5) 1) 2)))	; 1 / golden-section
    (lambda (f x0 x1 prec)
      (cond ((not (procedure? f)) (throw 'type-error gss 'procedure? f))
	    ((not (number? x0)) (throw 'type-error gss 'number? x0))
	    ((not (number? x1)) (throw 'type-error gss 'number? x1))
	    ((>= x0 x1) (throw 'type-error gss x0 'not '< x1)))
      (let ((stop?
	     (cond
	      ((procedure? prec) prec)
	      ((number? prec)
	       (if (>= prec 0)
		   (lambda (x0 x1 a b fa fb count) (<= (abs (- x1 x0)) prec))
		   (if (integer? prec)
		       (lambda (x0 x1 a b fa fb count) (>= count (- prec)))
		       (throw 'type-error  gss 'integer? prec))))
	      (else (throw 'type-error  gss 'procedure? prec))))
	    (a0 (+ x0 (* (- x1 x0) (- 1 r))))
	    (b0 (+ x0 (* (- x1 x0) r)))
	    (delta #f)
	    (fmax #f)
	    (fmin #f))
	(let loop ((left x0)
		   (right x1)
		   (a a0)
		   (b b0)
		   (fa (f a0))
		   (fb (f b0))
		   (count 1))
	  (define finish
	    (lambda (x fx)
	      (if (> fx fmin) (warn gss fx 'not 'min (list '> fmin)))
	      (if (and (> count 9) (or (eqv? x0 left) (eqv? x1 right)))
		  (warn gss 'min 'not 'found))
	      (cons x fx)))
	  (case count
	    ((1)
	     (set! fmax (max fa fb))
	     (set! fmin (min fa fb)))
	    ((2)
	     (set! fmin (min fmin fa fb))
	     (if (= fmax fa fb) (throw 'misc-error  gss 'flat? fmax)))
	    (else
	     (set! fmin (min fmin fa fb))))
	  (cond ((stop? left right a b fa fb count)
		 (if (< fa fb)
		     (finish a fa)
		     (finish b fb)))
		((< fa fb)
		 (let ((a-next (+ left (* (- b left) (- 1 r)))))
		   (cond ((and delta (< delta (- b a)))
			  (finish a fa))
			 (else (set! delta (- b a))
			       (loop left b a-next a (f a-next) fa
				     (+ 1 count))))))
		(else
		 (let ((b-next (+ a (* (- right a) r))))
		   (cond ((and delta (< delta (- b a)))
			  (finish b fb))
			 (else (set! delta (- b a))
			       (loop a right b b-next fb (f b-next)
				     (+ 1 count))))))))))))
 ;;; arch-tag: ceed457c-b54f-48c9-93c6-ed4210f4d82b
