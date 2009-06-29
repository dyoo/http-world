#lang scheme/base
(require web-server/servlet
         web-server/servlet-env
         scheme/local
         scheme/list
         lang/prim)

;; A prototype http-world.
;;
;;
;; (big-bang 0 [on-http (lambda (w req) w) 
;;                      (lambda (w req) (number->string w))])
;;
;; should bring up a very simple server that responds to requests.


 
(define-struct handler:http-world (world-update-f response-f))
(define-struct handler:tick (delay world-update-f))



;; request-lookup: request string -> string
;; Looks up the value of the key in the request; if not there, throws an exception.
(define (request-lookup a-request a-key)
  (extract-binding/single (string->symbol a-key) (request-bindings a-request)))


;; request-has?: request string -> boolean
;; Produces true if the request has a value for the given key.
(define (request-has? a-request a-key)
  (exists-binding? (string->symbol a-key) (request-bindings a-request)))




(define (big-bang initial-world . handlers)
  (local [(define config-elements (foldl (lambda (h config)
                                           (h config))
                                         empty
                                         handlers))
          
          (define world initial-world)
          ;; FIXME: use channels to enforce critical region.
          
          
          (define (handler-start! a-handler)
            (cond
              [(handler:http-world? a-handler)
               (local [(define world-update-f (handler:http-world-world-update-f 
                                               a-handler))
                       (define response-f (handler:http-world-response-f 
                                           a-handler))]
                 (thread (lambda ()
                           (define (handler request)
                             (local [(define new-world (world-update-f world request))
                                     (define a-response (response-f world request))]
                               (set! world new-world)
                               a-response))
                           (serve/servlet handler))))]
              
              [(handler:tick? a-handler)
               (thread (lambda ()
                         (let loop ()
                           (sleep (handler:tick-delay a-handler))
                           (local [(define new-world ((handler:tick-world-update-f a-handler) world))]
                             (set! world new-world))
                           (loop))))]))]
    (for-each handler-start! config-elements)))



(define (on-http world-update response-f)
  (lambda (config)
    (cons (make-handler:http-world world-update response-f)
          config)))
          


(define (on-tick delay world-update)
  (lambda (config)
    (cons (make-handler:tick delay world-update)
          config)))







(define (simple-test)
  (local [(define-struct world (hits seconds))]
    (big-bang (make-world 0 0) 
              
              (on-tick 1 (lambda (w) (make-world (world-hits w)
                                                 (add1 (world-seconds w)))))
              
              (on-http (lambda (w req) (make-world (add1 (world-hits w))
                                                     (world-seconds w)))
                       (lambda (w req) 
                           (string-append "Hello world, I see "
                                          (number->string (world-hits w))
                                          ".  "
                                          (number->string (world-seconds w))
                                          " seconds have passed since startup."))))))



(provide-primitive big-bang)
(provide-primitive request-lookup)
(provide-primitive request-has?)

(provide-higher-order-primitive on-http (world-update response-generator))

(provide-higher-order-primitive on-tick (delay handler))