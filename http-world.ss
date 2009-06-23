#lang scheme/base
(require web-server/servlet
         web-server/servlet-env
         scheme/local
         scheme/list
         lang/prim)

 
(define-struct http-world-function (world-update-f response-f))



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
(provide-higher-order-primitive on-http (world-update response-generator))
