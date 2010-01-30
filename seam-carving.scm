#!/usr/bin/env gosh
;; -*- coding: utf-8 -*-

(use srfi-1)  ;; List library
(use srfi-13) ;; String library
(use srfi-43) ;; Vector library
(use gauche.parseopt)
(use binary.io)

(define (load-image path)
  "Load ppm"
  'todo)

(define (pixel:create r g b)
  (list r g b))

(define (pixel:get accessor pixel . rest)
  (let ((val
	 (if (list? pixel) (accessor pixel) pixel)))
    (if (null? rest)
	val
	(cond ((< val 0) 0)
	      ((> val 255) 255)
	      (else val)))))

(define (pixel:r pixel . rest)
  (apply pixel:get car pixel rest))

(define (pixel:g pixel . rest)
  (apply pixel:get cadr pixel rest))

(define (pixel:b pixel . rest)
  (apply pixel:get caddr pixel rest))

(define (pixel:scale scale pixel)
  (list (* scale (car pixel))
	(* scale (cadr pixel))
	(* scale (caddr pixel))))

(define (pixel:scale! scale pixel)
  (let ((x (car pixel)) (xs (cdr pixel)))
    (set-car! pixel (round->exact (* scale x)))
    (let ((x (car xs)) (xs_ (cdr xs)))
      (set-car! xs (round->exact (* scale x)))
      (let ((x (car xs_)))
	(set-car! xs_ (round->exact (* scale x))))))
  pixel)

(define (pixel:+ . pixels)
  (let loop((r 0) (g 0) (b 0) (pixels pixels))
    (if (null? pixels)
	(list r g b)
	(let1 pixel (car pixels)
	  (loop (+ r (car pixel))
		(+ g (car pixel))
		(+ b (car pixel))
		(cdr pixels))))))

(define (pixel:- pixel . pixels)
  (let loop((r (car pixel)) (g (cadr pixel)) (b (caddr pixel))
	    (pixels pixels))
    (if (null? pixels)
	(list r g b)
	(let1 pixel (car pixels)
	  (loop (+ r (car pixel))
		(+ g (car pixel))
		(+ b (car pixel))
		(cdr pixels))))))

(define (pixel:abs! pixel)
  (let ((x (car pixel)) (xs (cdr pixel)))
    (set-car! pixel (abs x))
    (let ((x (car xs)) (xs_ (cdr xs)))
      (set-car! xs (abs x))
      (let ((x (car xs_)))
	(set-car! xs_ (abs x)))))
  pixel)

(define (pixel:abs pixel)
  (pixel:abs! (reverse (reverse pixel))))

(define (pixel:norm pixel)
  (round->exact (sqrt (fold (lambda (x y)
			      (+ (* x x) y))
			    0 pixel))))

(define (image:get-pixel image x y . rest)
  (guard (err
	  (else
	   (when (null? rest)
	     (raise err))
	   (set! x (cond ((< x 0) 0)
			 ((>= x (image:width image))
			  (- (image:width image) 1))
			 (else x)))
	   (set! y (cond ((< y 0) 0)
			 ((>= y (image:height image))
			  (- (image:height image) 1))
			 (else y)))
	   (vector-ref
	    (vector-ref (cdr (assq 'data image)) y) x)))
    (vector-ref (vector-ref (cdr (assq 'data image)) y) x)))

(define (image:set-pixel image x y pixel)
  (let1 row (vector-ref (cdr (assq 'data image)) y #f)
    (if row
	(vector-set! row x pixel)
	#f)))

(define (image:data image)
  (cdr (assq 'data image)))

(define (image:width image)
  (cdr (assq 'width image)))

(define (image:height image)
  (cdr (assq 'height image)))

(define (image::load-ppm-raw path)
  (with-input-from-file path
    (lambda ()
      (let ((fmt (read-line))
	    (comment (if (eq? #\# (peek-char ))
			 (read-line)
			 #f))
	    (size (read-line))
	    (max-brightness (read-line)))
	(format #t "fmt: ~s\ncomment: ~s\nsize: ~s\nmax-brightness: ~s\n"
		fmt comment size max-brightness)
	(unless (equal? "P6" fmt)
	  (error "Invliad format file '~s': '~s'" path fmt))
	(receive (width height) (image::parse-size size)
	  (let ((image (image::create width height)))
	    (dotimes (y height)
	      (dotimes (x width)
		(let ((r (read-byte))
		      (g (read-byte))
		      (b (read-byte)))
		  (let1 pixel (pixel:create r g b)
		    (image:set-pixel image x y pixel)))))
	    image))
	))))

(define (image::save-ppm-raw image path)
  (with-output-to-file path
    (lambda ()
      (print "P6")
      (format #t "~d ~d\n" (image:width image) (image:height image))
      (print "255")
      (vector-for-each
       (lambda (idx row)
	 (vector-for-each
	  (lambda (idx pixel)
	    (write-byte (pixel:r pixel #t))
	    (write-byte (pixel:g pixel #t))
	    (write-byte (pixel:b pixel #t)))
	  row))
       (image:data image))))
      #t)

(define (image::create width height . rest)
  (let1 data (if (not (null? rest))
		 (car rest)
		 (let1 rows (make-vector height #f)
		   (do ((i 0 (+ i 1)))
		       ((>= i height) rows)
		     (vector-set! rows i (make-vector width #f)))))
    `((width . ,width) (height . ,height) (data . ,data))))

(define (image::parse-size size-str)
  (with-input-from-string size-str
    (lambda ()
      (let ((width (read))
	    (height (read)))
	(if (and (integer? width)
		 (integer? height))
	    (values width height)
	    (error "Invalid size string: ~s" size-str))))))

(define (image::sc-horizontally image)
  "Carve a seam horizontally."
  'todo)

(define (image::sc-vertically image)
  "Carve a seam vertically."
  (let* ((energy-map (image::make-energy-map image))
	 (seam (image::find-vertical-seam energy-map)))
    (image::carve-seam image seam))
  #t)

(define (image::make-energy-map image . rest)
  (let1 type (if (null? rest) 'simple-diff (car rest))
    (case type
      ((simple-diff)
       (image::make-energy-map-simple-diff image))
      (else
       (error "No such energy type: ~s" type)))))

(define (image::sobel-operator-x image x y)
  (pixel:- (pixel:+ (image:get-pixel image (+ x 1) (- y 1) #t)
		    (pixel:scale 2 (image:get-pixel image (+ x 1) y #t))
		    (image:get-pixel image (+ x 1) (+ y 1) #t))
	   (image:get-pixel image (- x 1) (- y 1) #t)
	   (pixel:scale 2 (image:get-pixel image (- x 1) y #t))
	   (image:get-pixel image (- x 1) (+ y 1) #t)))

(define (image::sobel-operator-y image x y)
  (pixel:- (pixel:+ (image:get-pixel image (- x 1) (+ y 1) #t)
		    (pixel:scale 2 (image:get-pixel image x (+ y 1) #t))
		    (image:get-pixel image (+ x 1) (+ y 1) #t))
	   (image:get-pixel image (- x 1) (- y 1) #t)
	   (pixel:scale 2 (image:get-pixel image x (- y 1) #t))
	   (image:get-pixel image (+ x 1) (- y 1) #t)))

(define (image::sobel-operator-abs-x image x y)
  (- (+ (pixel:norm (image:get-pixel image (+ x 1) (- y 1) #t))
	(* 2 (pixel:norm (image:get-pixel image (+ x 1) y #t)))
	(pixel:norm (image:get-pixel image (+ x 1) (+ y 1) #t)))
     (pixel:norm (image:get-pixel image (- x 1) (- y 1) #t))
     (* 2 (pixel:norm (image:get-pixel image (- x 1) y #t)))
     (pixel:norm (image:get-pixel image (- x 1) (+ y 1) #t))))

(define (image::sobel-operator-abs-y image x y)
  (- (+ (pixel:norm (image:get-pixel image (- x 1) (+ y 1) #t))
	(* 2 (pixel:norm (image:get-pixel image x (+ y 1) #t)))
	(pixel:norm (image:get-pixel image (+ x 1) (+ y 1) #t)))
     (pixel:norm (image:get-pixel image (- x 1) (- y 1) #t))
     (* 2 (pixel:norm (image:get-pixel image x (- y 1) #t)))
     (pixel:norm (image:get-pixel image (+ x 1) (- y 1) #t))))

(define (image::make-energy-map-simple-diff-rgb image)
  "Simple differential"
  (let ((energy-map (image::create (image:width image)
				  (image:height image)))
	(max-energy 0))
    (dotimes (y (image:height image))
      (dotimes (x (image:width image))
	(let ((dx (pixel:abs! (image::sobel-operator-x image x y)))
	      (dy (pixel:abs! (image::sobel-operator-y image x y))))
	  (let1 e (pixel:+ dx dy)
	    (image:set-pixel energy-map x y e)
	    (when (> (pixel:r e) max-energy)
	      (set! max-energy (pixel:r e)))
	    (when (> (pixel:g e) max-energy)
	      (set! max-energy (pixel:g e)))
	    (when (> (pixel:b e) max-energy)
	      (set! max-energy (pixel:b e)))))))
    
    ;; normalize
    (when (> max-energy 255)
      (let1 scale (/ 255 max-energy)
	(dotimes (y (image:height energy-map))
	  (dotimes (x (image:width energy-map))
	    (image:set-pixel
	     energy-map x y
	     (pixel:scale! scale (image:get-pixel energy-map x y)))))))
    energy-map
    ))

(define (image::make-energy-map-simple-diff image)
  "Simple differential"
  (let ((energy-map (image::create (image:width image)
				  (image:height image)))
	(max-energy 0))
    (dotimes (y (image:height image))
      (dotimes (x (image:width image))
	(let ((dx (abs (image::sobel-operator-abs-x image x y)))
	      (dy (abs (image::sobel-operator-abs-y image x y))))
	  (let1 e (round->exact (+ dx dy))
	    (image:set-pixel energy-map x y e)
	    (when (> (pixel:r e) max-energy)
	      (set! max-energy (pixel:r e)))
	    (when (> (pixel:g e) max-energy)
	      (set! max-energy (pixel:g e)))
	    (when (> (pixel:b e) max-energy)
	      (set! max-energy (pixel:b e)))))))
    
    ;; normalize
    (when (> max-energy 255)
      (let1 scale (/ 255 max-energy)
	(dotimes (y (image:height energy-map))
	  (dotimes (x (image:width energy-map))
	    (image:set-pixel energy-map x y
			     (round->exact 
			      (* scale (image:get-pixel energy-map x y))))))))
      energy-map
      ))

(define (image::seam-carving image width height)
  (let ((width-delta (- (image:width image) width))
	(height-delta (- (image:height image) height)))
    (when (< width-delta 0)
      (error "Width enlarging is not supported."))
    (when (< height-delta 0)
      (error "Height enlarging is not supported."))
    (dotimes (i width-delta)
      (image::sc-vertically image))
    (dotimes (i height-delta)
      (image::sc-horizontally))
    image))

(define (main args)
  0)
