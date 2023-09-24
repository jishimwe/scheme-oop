;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Final Step : Interface

(display "   ======================== Final Step ========================")
(newline)
(display "   Inteface")
(newline)

(define (error reason . args) ; refs https://srfi.schemers.org/srfi-23/srfi-23.html
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg)
              (display " ")
              (write arg))
            args)
  (newline)
  (scheme-report-environment 5))

(define (object)
  (define (super . message)
    'nil)
  
  (define (type) 'object)

  (define (errArg . dump) (display "Message not understood")
    (newline))

  (define (set-self! s)
    (set! self s))

  (define (self m)
    (case m
      ('type type)
      ('setself set-self!)
      ('super super)
      (else errArg)
      )
    )
  self
  )

(define (point x y)
  (define o (object))
  
  (define (super . message)
    (cond ((null? message) (o message))
          ((null? (car message)) (o 'type))
          (else (o (car message)))
          )
    )
  ; Testing the late self binding implementation
  (define (dummy-fun)
    (display "IN POINT")
    (newline))
  
  (define (late-binding-test)
    (send self 'dummy-fun)
    )
  
  (define (getx) x)

  (define (gety) y)

  (define (setx! a) (set! x a))

  (define (sety! a) (set! y a))

  (define (type) 'point)

  (define (info) (list 'point x y))

  (define (info4) (list (send self 'type) x y))

  (define (add p)
    (point (+ ((p 'getx)) x) (+ ((p 'gety)) y))
    )

  (define (substract p)
    (point (- ((p 'getx)) x) (- ((p 'gety)) y))
    )

  (define (div d)
    (point (/ x d) (/ y d))
    )

  (define (mul m)
    (point (* x m) (* x m))
    )

  (define (point-eq? p) ; test if p is equal to this point
    (cond ((eq? ((p 'getx)) x)
           (cond ((eq? ((p 'gety)) y) #t)
                 (else #f))
           )
          (else #f)
          )
    )

  (define (set-self! s)
    (set! self s)
    ((o 'setself) s))
  
  (define (self p) ; Renaming dispatch into self
    (case p
      ('getx getx)
      ('gety gety)
      ('type type)
      ('info info4)
      ('info1 info1)
      ('setx! setx!)
      ('sety! sety!)
      ('add add)
      ('substract substract)
      ('div div)
      ('mul mul)
      ('errArg (super))
      ('super (super))
      ('test late-binding-test)
      ('dummy-fun dummy-fun)
      ('setself set-self!)
      ('point-eq? point-eq?)
      (else (super))
      )
    )
  self
  )

(define (color-point x y color)
  (define p (point x y))
  
  (define (super . message)
    (cond ((null? message) (p 'super))
          ((null? (car message)) (p 'type))
          (else (p (car message)))
          )
    )

  (define (dummy-fun)
    (display "IN COLOR-POINT ")
    (newline)
    )
  
  (define (type) 'color-point)

  (define (setcolor! c) (set! color c))

  (define (getcolor) color)

  (define (info) (list (type) ((super 'getx)) ((super 'gety)) (getcolor)))

  (define (info4) (append ((super 'info)) (list color))) ; info using super calls

  (define (set-self! s)
    (set! self s)
    ((p 'setself) s))

  (define (self cp)
    (case cp
      ('setcolor! setcolor!)
      ('getcolor getcolor)
      ('type type)
      ('info info4)
      ('null (super 'errArg))
      ('super (super 'type))
      ('dummy-fun dummy-fun)
      ('setself set-self!)
      (else (super cp))
      )
    )
  self
  )

(define (shape-interface point1 point2 point3 pointList) ; not set-self -> super call -> 'interface
  (define o (object))
  
  (define (super . message)
    (cond ((null? message) (o message))
          (else 'interface)
          )
    )

  (define (type) (error "Not implemented" 'type))

  (define (add-point! p) (error "Not implemented" 'add-point p))

  (define (del-point! p) (error "Not implemented" 'del-point p))

  (define (get-points) (error "Not implemented" 'get-points))

  (define (info) (error "Not implemented" 'info))

  (define (self sp)
    (case sp
      ('type type)
      ('add-point! add-point!)
      ('del-point! del-point!)
      ('get-points get-points)
      ('info info)
      (else (super sp))
      )
    )
  self
  )

(define (shape point1 point2 point3 . pointList)
  (define s-i (shape-interface point1 point2 point3 pointList))

  (define points (cond ((null? pointList) (list point1 point2 point3))
                       (else (append (list point1 point2 point3) pointList))
                       )
    )

  (define (super . message)
    (cond ((null? message) (s-i message))
          (else (s-i (car message)))
          )
    )

  (define (set-last l el) ; add an element at the end of a list
    (cond ((null? (cdr l)) (set-cdr! l (list el)))
          (else (set-last (cdr l) el))
          )
    )

  (define (list-info list ) ; listing the info of the points
    (cond ((null? (cdr list)) (cons (((car list) 'info)) '()))
          (else (cons (((car list) 'info))
                (list-info (cdr list)))
                )
          )
    )

  (define (list-del l p) ; remove a point (p) from list of points
    (cond ((null? (car l)) l)
          (((p 'point-eq?) (car l))
           (set-car! l (cdr l))
           (set-cdr! l '())
           )
          (else (list-del (cdr l) p))
          )
    )

  (define (type) 'shape)

  (define (add-point! p) (set-last points p))

  (define (del-point! p)
    (list-del points p)
    (set! points (car points))
    )

  (define (info) 
    (list (type) (list-info points))
    )

  (define (set-self! s)
    (set! self s))

  (define (self sp)
    (case sp
      ('type type)
      ('add-point! add-point!)
      ('del-point! del-point!)
      ('info info)
      ('setself set-self!)
      (else (super sp))
      )
    )
  self
  )

(define (errType . dump) (display "Innapropriate receiver object")
  (newline)
  (display "dump  -  ")
  (display dump)
  (newline))

(define (errArgs . dump) (display "Message not understood")
  (newline)
  (display "dump  -  ")
  (display dump)
  (newline))

(define (typeCheck? t)
  (cond ((procedure? t)
         (case ((t 'type))
           ('point #t)
           ('object #t)
           ('color-point #t)
           ('shape #t)
           (else #f))
         )
        (else #f)
        )
  )

; For when a sub-class return an object
; We create the actual super class with the data changed in the sub-class
(define (object-wrapper obj subObj) 
  (cond ((procedure? subObj)
         (cond ((eq? ((obj 'type)) ((subObj 'type))) subObj)
               ((eq? ((obj 'type)) 'color-point)
                (new (eval ((obj 'type)) (interaction-environment))
                 ((subObj 'getx))
                 ((subObj 'gety))
                 ((obj 'getcolor)))
                )
               ((eq? ((obj 'super)) 'nil) subObj)
               (else subObj)
               )
         )
        (else subObj)
        )
  )
  

(define (method-lookup receiver message)
  (receiver message)
  )

(define (send receiver message . args)
  (cond ((null? receiver) errType)
        ((null? message) errArgs)
        ((typeCheck? receiver) ; Check if it's a valid object
         (object-wrapper receiver
                          (apply (method-lookup receiver message) args))
         )
        (else (errType receiver))
        )
  )

(define (new object . args)
  (cond ((procedure? object)
         (let ((obj (apply object args)))
           ((obj 'setself) obj)
           obj)
         )
        (else (errType))
        )
  )

(define (type-cast oldObj newObj message)
  (cond ((procedure? oldObj)
         (cond ((procedure? newObj)
                (send newObj message))
               (else (errType))
               )
         )
         (else (errType))
         )
  )

(define o (new object))
(send o 'type)
(send o 'foo) ; should display "Message not understood"
(send 'not-object 'type) ; should display "Innapropriate receiver object"
(newline)
  
(define p1 (new point 1 2))
(define p2 (new point 3 4))
(define p3 (new point 7 12))

(send p1 'getx) ; 1
(send p1 'gety) ; 2
(send p2 'getx) ; 3
(send p2 'gety) ; 4

(send p1 'setx! 5)
(send p1 'sety! 5)
(send p1 'info) ; should display (point 5 5)

(define p (send p1 'add p2))
(send p 'info) ; should display (point  8 9)
(newline)

(define cp (new color-point 5 6 'red))
(send cp 'type)
(send cp 'info) ; should display (color-point 5 6 red)
(send cp 'getx)

(send cp 'sety! 11)
(send cp 'setcolor! 'blue)
(send cp 'info) ; should display (color 5 11 blue)

(define cp1 (send cp 'add (color-point 1 2 'green)))
(send cp1 'type)
(send cp1 'getx)
(send cp1 'gety)
(send cp1 'getcolor)
(send cp1 'info) ; should display (color-point 6 13 blue)
(newline)

(send 'not-a-point 'info) ; should display ”Inappropriate receiver object”
(send p 'foo)  ; should display ”Message not understood”
(send p 'bar 2)  ; should display ”Message not understood”

(send p 'test) ; should disply "IN POINT"
(send cp 'test) ; should display "IN COLOR-POINT"
(newline)

(define triangle (new shape p p1 p2))

(send triangle 'info) ; should display "(shape ((point 8 9) (point 5 5) (point 3 4)))"
(send triangle 'get-points) ; should give an not implemented error
(send triangle 'add-point! p3) ; adding (point 7 12)
(send triangle 'info) ; should display "(shape ((point 8 9) (point 5 5) (point 3 4) (point 7 12)))"
(send triangle 'del-point! p) ; deleting (point 8 9)
(send triangle 'info) ; should display "(shape ((point 5 5) (point 3 4) (point 7 12)))"

; error -> expected a procedure that can be applied to arguments given: interface
(define quad (new shape-interface p p1 p2 p3))


(newline)
(display "   ======================== Final Step DONE ========================")
(newline)