#lang scheme/base
(require web-server/servlet
         web-server/servlet-env
         scheme/local
         scheme/list
         lang/prim)

 
(define-struct http-world-function (world-update-f response-f))


;; request-lookup: request string -> string
;; Looks up the value of the key in the request; if not there, throws an exception.
(define (request-lookup a-request a-key)
  (extract-binding/single (string->symbol a-key) (request-bindings a-request)))

;; request-has?: request string -> boolean
;; Produces true if the request has a value for the given key.
(define (request-has? a-request a-key)
  (exists-binding? (string->symbol a-key) (request-bindings a-request)))




(define (big-bang initial-world . handlers)
  (local [(define config (first (foldl (lambda (h config)
                                         (h config))
                                       empty
                                       handlers)))
          (define world-update-f (http-world-function-world-update-f config))
          (define response-f (http-world-function-response-f config))
          
          (define world initial-world)
  
          (define (handler request)
            (local [(define new-world (world-update-f world request))
                    (define a-response (response-f world request))]
              (set! world new-world)
              a-response))]

    (serve/servlet handler)))



(define (on-http world-update response-f)
  (lambda (config)
    (cons (make-http-world-function world-update response-f)
          config)))
          



(define (simple-test)
  (big-bang 0 (on-http (lambda (w req) (add1 w))
                       (lambda (w req) 
                         (string-append "Hello world, I see "
                                        (number->string w))))))



(provide-primitive big-bang)
(provide-primitive request-lookup)
(provide-primitive request-has?)

(provide-higher-order-primitive on-http (world-update response-generator))
