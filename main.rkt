#lang racket/gui

(require (prefix-in screen: "config/screen.rkt"))

(require 2htdp/image)
(require 2htdp/universe)

;; Struct
(struct world (tiles))
(struct fixed-image (image x y))

;; tile type
(define normal (square 20 "solid" "red"))
(define start  (square 20 "solid" "slateblue"))

;; tiletype -> tile
(define (tile-type->image tile-type)
  (match tile-type
    ['normal normal]
    ['start start]))

;; Making random tile list
(define (make-random-tiles-list x y)
  (let ([rnd (random (* x y))])
    (shuffle (append '(start) (make-list (- (* x y) 1) 'normal)))))


;; generate a list of fixed images
(define (generate-random-tiles x y)
  (map (lambda (type pos)
         (make-fixed-image (tile-type->image type) pos))
       (make-random-tiles-list x y)
       (stream->list (in-range 0 (* x y)))))
         

;; simple helper to 1D array to 2D array
(define (make-fixed-image image idx)
  (fixed-image image (remainder idx 8) (quotient idx 8)))

;; match click event
(define (mouse-click state x y event)
  (match event
    ["button-up" (world (cdr (world-tiles state)))]
    [x state]))
  

;; Create and image in folding a list of images
(define (tile-to-image tiles offset)
  (foldl (lambda (tile acc)
           (place-image (fixed-image-image tile) (+ offset (* 21 (fixed-image-x tile))) (+ offset (* 21 (fixed-image-y tile))) acc))
         (empty-scene 640 480)
         tiles))


;; callback called when redrawing
(define (display w)
  (match (world-tiles w)
    [x (tile-to-image x 20)]))

;; when timer is called
(define (toc w)
  (println "ding dong")
  w)

;; initial state
(define (world0)
  (world (generate-random-tiles 8 6)))

(big-bang (world0)
  (on-tick toc 1)
  (on-mouse mouse-click)
  (on-draw display))