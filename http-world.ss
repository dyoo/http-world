#lang scheme/base
(require web-server/servlet
         web-server/servlet-env
         scheme/local
         scheme/list
         scheme/match
         lang/prim)

;; A prototype http-world.
;;
;;
;; (big-bang 0 [on-http (lambda (w req) w) 
;;                      (lambda (w req) (number->string w))])
;;
;; should bring up a very simple server that responds to requests.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The handler structures.

(define-struct handler:request->raw (world-update-f response-f))
(define-struct handler:request->html+css (world-update-f html-response-f css-response-f))
(define-struct handler:tick (delay world-update-f))
(define-struct handler:stop-when (stop-when-f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; request-lookup: request string -> string
;; Looks up the value of the key in the request; if not there, throws an exception.
(define (request-lookup a-request a-key)
  (extract-binding/single (string->symbol a-key) (request-bindings a-request)))


;; request-has?: request string -> boolean
;; Produces true if the request has a value for the given key.
(define (request-has? a-request a-key)
  (exists-binding? (string->symbol a-key) (request-bindings a-request)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; on-http/raw: (world request -> world) (world request -> response) -> handler
(define (on-http/raw world-update response-f)
  (lambda (config)
    (cons (make-handler:request->raw world-update response-f)
          config)))
          

;; on-http: world (world request -> world) (world request -> html) (world request -> css) -> handler
(define (on-http world-update html-response-f css-response-f)
  (lambda (config)
    (cons (make-handler:request->html+css world-update html-response-f css-response-f)
          config)))

  
;; on-tick: number (world -> world) -> handler
(define (on-tick delay world-update)
  (lambda (config)
    (cons (make-handler:tick delay world-update)
          config)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; big-bang: world (listof handlers) -> world
(define (big-bang initial-world . handlers)
  (local [(define config-elements (foldl (lambda (h config)
                                           (h config))
                                         empty
                                         handlers))
          
          (define world initial-world)

          ;; FIXME: use a channel to enforce the critical region.
          
          (define (handler-start! a-handler)
            (match a-handler
              [(struct handler:request->raw (world-update-f response-f))
                 (thread (lambda ()
                           (define (handler request)
                             (local [(define new-world (world-update-f world request))
                                     (define a-response (response-f world request))]
                               (set! world new-world)
                               a-response))
                           (serve/servlet handler)))]

              [(struct handler:request->html+css (world-update-f http-response-f css-response-f))
               (void)
               ;; fixme
               ]

              [(struct handler:tick (delay world-update-f))
               (thread (lambda ()
                         (let loop ()
                           (sleep delay)
                           (local [(define new-world (world-update-f world))]
                             (set! world new-world))
                           (loop))))]))]
    (for-each handler-start! config-elements)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A small function to exercise the framework.
(define (simple-test)
  (local [(define-struct world (hits seconds))]
    (big-bang (make-world 0 0) 
              
              (on-tick 1 (lambda (w) (make-world (world-hits w)
                                                 (add1 (world-seconds w)))))
              
              (on-http/raw (lambda (w req) (make-world (add1 (world-hits w))
                                                       (world-seconds w)))
                           (lambda (w req) 
                             (string-append "Hello world, I've seen "
                                            (number->string (world-hits w))
                                            ".  "
                                            (number->string (world-seconds w))
                                            " seconds have passed since startup."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-primitive big-bang)
(provide-primitive request-lookup)
(provide-primitive request-has?)

(provide-higher-order-primitive on-http (world-update http-response-generator css-response-generator))
(provide-higher-order-primitive on-http/raw (world-update response-generator))
(provide-higher-order-primitive on-tick (delay handler))