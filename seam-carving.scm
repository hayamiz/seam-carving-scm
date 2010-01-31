#!/usr/bin/env gosh
;; -*- coding: utf-8 -*-

(use srfi-1)  ;; List library
(use srfi-13) ;; String library
(use srfi-27) ;; Random library
(use srfi-43) ;; Vector library
(use gauche.parseopt)
(use gauche.process)
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
  (let ((x (car pixel))
	(y (cadr pixel))
	(z (caddr pixel)))
    (round->exact
     (sqrt (+ (* x x) (* y y) (* z z))))))

(define (image:get-pixel image x y . rest)
  (let ((width (image:width image))
	(height (image:height image)))
    (unless (null? rest)
      (unless (< -1 x width)
	(set! x (cond ((< x 0) 0)
		      ((>= x width)
		       (- width 1))
		      (else x))))
      (unless (< -1 y height)
	(set! y (cond ((< y 0) 0)
		      ((>= y height)
		       (- height 1))
		      (else y)))))
    (vector-ref (vector-ref (cdr (assq 'data image)) y) x)))

(define (image:row-get-pixel row x . rest)
  (let ((width (vector-length row)))
    (unless (null? rest)
      (unless (< -1 x width)
	(set! x (cond ((< x 0) 0)
		      ((>= x width)
		       (- width 1))
		      (else x))))
    (vector-ref row x))))

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

(define (image:set-width! image width)
  (set-cdr! (assq 'width image) width))

(define (image:set-height! image height)
  (set-cdr! (assq 'height image) height))

(define (image::load-ppm-raw input)
  (let1 input-port (if (string? input)
		       (open-input-file input)
		       input)
    (unwind-protect
	(with-input-from-port input-port
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
	      )))
      (when (string? input)
	(close-input-port input-port)))))

(define (image::save-ppm-raw image output)
  (let1 output-port (if (string? output)
			(open-output-file output)
			(begin
			  (let1 proc (run-process `("display" "-")
						  :input :pipe
						  :wait #f)
			    (process-input proc))))
    (unwind-protect
	(with-output-to-port output-port
	  (lambda ()
	    (print "P6")
	    (format #t "~d ~d\n" (image:width image) (image:height image))
	    (print "255")
	    (dotimes (y (image:height image))
	      (dotimes (x (image:width image))
		(let1 pixel (image:get-pixel image x y)
		  (write-byte (pixel:r pixel #t))
		  (write-byte (pixel:g pixel #t))
		  (write-byte (pixel:b pixel #t)))))))
      (begin
	(close-output-port output-port)
	(unless (string? output)
	  (process-wait-any)))))
  #t)


(define (image::create width height . rest)
  (let1 data (if (not (null? rest))
		 (car rest)
		 (let1 rows (make-vector height #f)
		   (do ((i 0 (+ i 1)))
		       ((>= i height) rows)
		     (vector-set! rows i (make-vector width #f)))))
    `((width . ,width) (height . ,height) (data . ,data))))

(define (image::clone image)
  (let ((image-data (image:data image))
	(data (make-vector (image:height image))))
    (dotimes (y (image:height image))
      (vector-set! data y (vector-copy (vector-ref image-data y))))
    (image::create (image:width image)
		   (image:height image)
		   data)))

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

(define (image::sc-vertically image size)
  "Carve a seam vertically."
  (dotimes (i size)
    (let ((energy-map (time (image::make-energy-map image))))
      (let1 seam (time (image::find-vertical-seam energy-map))
	(image::carve-seam image seam)
	(image:set-width! image (- (image:width image) 1))
	(image::carve-seam energy-map seam)
	(image:set-width! energy-map (- (image:width energy-map) 1))
	)))
  #t)

(define (image::sc-vertically-file input output width-shrink)
  (let1 image (image::load-ppm-raw input)
    (image::sc-vertically image
			  (or width-shrink
			      (round->exact
			       (/ (image:width image) 2))))
    (image::save-ppm-raw image output)))

(define (image::carve-seam image seam)
  (let* ((width (image:width image))
	 (new-width (- width 1)))
    (vector-map!
     (lambda (y row)
       (let ((seam-x (caar seam))
	     (seam-y (cdar seam))
	     (new-row (vector-copy row 0 new-width)))
	 (vector-copy! new-row seam-x
		       row (+ seam-x 1) width)
	 new-row))
     (image:data image)))
  #t)

(define (image::mark-seam image seam)
  (dotimes (y (image:height image))
    (let ((seam-x (caar seam))
	  (seam-y (cdar seam)))
      (unless (eq? y seam-y)
	(error ""))
      (image:set-pixel image seam-x seam-y
		       (pixel:create 255 0 0))
      (set! seam (cdr seam)))))

(define (image::find-vertical-seam energy-map)
  (let ((width (image:width energy-map))
	(height (image:height energy-map)))
    (let* ((range-width 50)
	   (init-search-begin
	    (random-integer (max (- width range-width) 0))
	    )
	   (init-search-end
	    (min (max (+ init-search-begin range-width)
		      (- width 1)))))
      (let1 start-x
	  (let loop((min-energy 0)
		    (min-x init-search-begin)
		    (x 0))
	    (if (<= x init-search-end)
		(if (> min-energy (image:get-pixel energy-map x 0))
		    (loop (image:get-pixel energy-map x 0) x (+ x 1))
		    (loop min-energy min-x (+ x 1)))
		min-x))
	(let ((cost-map (image::create width height)))
	  (image:set-pixel cost-map start-x 0 (cons 0 #f))
	  (let ((choose-prevline-minpixel
		 (lambda (x y)
		   (dec! y)
		   (let ((left (image:get-pixel cost-map (- x 1) y #f))
			 (center (image:get-pixel cost-map x y #f))
			 (right (image:get-pixel cost-map (+ x 1) y #f)))
		     (let ((min-x 0)
			   (min-cost +inf.0))
		       (when (and left (< (car left) min-cost))
			 (set! min-x (- x 1))
			 (set! min-cost (car left)))
		       (when (and center (<= (car center) min-cost))
			 (set! min-x x)
			 (set! min-cost (car center)))
		       (when (and right (< (car right) min-cost))
			 (set! min-x (+ x 1))
			 (set! min-cost (car right)))
		       (values min-cost min-x)
		       )))))
	    (do ((y 1 (+ y 1)))
		((>= y height))
	      (do ((x (max 0 (- start-x y)) (+ x 1)))
		  ((>= x (min (+ start-x y) width)))
		(receive (min-cost min-x)
		    (choose-prevline-minpixel x y)
		  (image:set-pixel
		   cost-map x y
		   (cons (+ (image:get-pixel energy-map x y) min-cost)
			 min-x))))
	      )

	    ;; cost map was build. backtrace it
	    ;; first, find end point
	    (let* ((min-energy-x (max 0 (+ 1 (- start-x height))))
		   (min-energy-cost-pixel (image:get-pixel cost-map
							   min-energy-x
							   (- height 1))))
	      (min (- (+ start-x height) 1)
		   (- width 1))
	      (do ((x (max 0 (+ 1 (- start-x height))) (+ x 1)))
		  ((>= x (min (- (+ start-x height) 1)
			      (- width 1))))
		(let ((cost-pixel (image:get-pixel cost-map x (- height 1))))
		  (when (< (car cost-pixel) (car min-energy-cost-pixel))
		    (set! min-energy-cost-pixel cost-pixel)
		    (set! min-energy-x x))))
	      (let loop((seam ())
			(x min-energy-x)
			(y (- height 1)))
		(if (< y 0)
		    seam
		    (let1 cost-pixel (image:get-pixel cost-map x y)
		      (loop (cons (cons x y) seam)
			    (cdr cost-pixel)
			    (- y 1))))))))))))

(define (image::make-energy-map image . rest)
  (apply image::make-energy-map-simple-diff image rest))

(define (image::sobel-operator-x image x y)
  (- (+ (image:get-pixel image (+ x 1) (- y 1) #t)
	(* 2 (image:get-pixel image (+ x 1) y #t))
	(image:get-pixel image (+ x 1) (+ y 1) #t))
     (image:get-pixel image (- x 1) (- y 1) #t)
     (* 2 (image:get-pixel image (- x 1) y #t))
     (image:get-pixel image (- x 1) (+ y 1) #t)))

(define (image::sobel-operator-y image x y)
  (- (+ (image:get-pixel image (- x 1) (+ y 1) #t)
	(* 2 (image:get-pixel image x (+ y 1) #t))
	(image:get-pixel image (+ x 1) (+ y 1) #t))
     (image:get-pixel image (- x 1) (- y 1) #t)
     (* 2 (image:get-pixel image x (- y 1) #t))
     (image:get-pixel image (+ x 1) (- y 1) #t)))

(define-syntax image::sobel-operator-horizontal
  (syntax-rules ()
    ((_ x-1y-1 x+1y-1 x-1y x+1y x-1y+1 x+1y+1)
     (- (+ x+1y-1 (* 2 x+1y) x+1y+1)
	x-1y-1 (* 2 x-1y) x-1y+1))))

(define-syntax image::sobel-operator-vertical
  (syntax-rules ()
    ((_ x-1y-1 xy-1 x+1y-1 x-1y+1 xy+1 x+1y+1)
     (- (+ x-1y+1 (* 2 xy+1) x+1y+1)
	x-1y-1 (* 2 xy-1) x+1y-1))))

(define (image::make-energy-map-simple-diff image . rest)
  "Simple differential"
  (let-optionals* rest
      ((normalize? #f))
    (let* ((width (image:width image))
	   (height (image:height image))
	   (norm-map
	    (image::create
	     (image:width image) (image:height image)
	     (vector-map
	      (lambda (y row)
		(vector-map
		 (lambda (x pixel)
		   (pixel:norm pixel))
		 row))
	      (image:data image))))
	   (energy-map (image::create (image:width image)
				      (image:height image)))
	   (max-energy 0)
	   (norm-map-data (image:data norm-map)))
      (let ((row-1 (vector-ref norm-map-data 0))
	    (row0  (vector-ref norm-map-data 0))
	    (row+1 (vector-ref norm-map-data 1)))
	(vector-for-each
	 (lambda (y row)
	   (let ((P-1 (lambda (x)
			(image:row-get-pixel
			 row-1 x #f)))
		 (P0 (lambda (x)
		       (image:row-get-pixel row0 x #f)))
		 (P+1 (lambda (x)
			(image:row-get-pixel
			 row+1 x #f))))
	     (let ((x-1y-1 (P-1 -1))  (xy-1 (P-1 0))  (x+1y-1 (P-1 1))
		   (x-1y   (P0  -1))  (xy   (P0  0))  (x+1y   (P0  1))
		   (x-1y+1 (P+1 -1))  (xy+1 (P+1 0))  (x+1y+1 (P+1 1)))
	       (vector-map!
		(lambda (x _)
		  (let ((dx (abs (image::sobel-operator-horizontal
				  x-1y-1 x+1y-1 x-1y x+1y x-1y+1 x+1y+1)))
			(dy (abs (image::sobel-operator-vertical
				  x-1y-1 xy-1 x+1y-1 x-1y+1 xy+1 x+1y+1))))
		    (let1 x+2 (+ x 2)
		      (when (< x width)
			(set! x-1y-1   xy-1)
			(set! x-1y     xy  )
			(set! x-1y+1   xy+1)
			(set! xy-1     x+1y-1)
			(set! xy       x+1y  )
			(set! xy+1     x+1y+1)
			(set! x+1y-1   (P-1 x+2))
			(set! x+1y     (P0  x+2))
			(set! x+1y+1   (P+1 x+2))))
		    (let1 e (+ dx dy)
		      (set! max-energy (max e max-energy))
		      e)))
		row)))
	   (set! row-1 row0)
	   (set! row0 row+1)
	   (set! row+1 (if (>= y (- height 2))
			   (vector-ref norm-map-data (- height 1))
			   (vector-ref norm-map-data (+ y 2)))))
	 (image:data energy-map)))
    
      ;; normalize
      (when (and #?=normalize? (> max-energy 255))
	(let1 scale (/ 255 max-energy)
	  (dotimes (y (image:height energy-map))
	    (dotimes (x (image:width energy-map))
	      (image:set-pixel energy-map x y
			       (round->exact 
				(* scale (image:get-pixel energy-map x y))))))))
      energy-map
      )))


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
  (let-args (cdr args)
      ((output "-o|output=s" #f)
       (width-shrink "-w|--width-shrink=i" #f)
       . restargs)
    (let-optionals* restargs
	((input (standard-input-port)))
      (image::sc-vertically-file input output width-shrink)))
  0)
