;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname example-counter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "http-world.ss")


(define (update w req)
  (add1 w))

(define (make-response w req)
  (string-append "Hello world, I see "
                 (number->string w)))

(big-bang 0 (on-http update make-response))