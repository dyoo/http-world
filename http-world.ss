#lang scheme/base
(require web-server/servlet
         web-server/servlet-env
         scheme/local
         scheme/list
         scheme/match
         scheme/string
         (only-in xml make-cdata)
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

;; A handler is something that takes a configuration and produces a new configuration.


;; on-http/raw: (world request -> world) (world request -> response) -> handler
(define (on-http/raw world-update response-f)
  (lambda (config)
    (when (findf handler:request->html+css? config)
      (error 'on-http/raw "Cannot define both an on-http/raw and on-http in a single big bang"))
    (cons (make-handler:request->raw world-update response-f)
          config)))


;; on-http: world (world request -> world) (world request -> html) (world request -> css) -> handler
(define (on-http world-update html-response-f css-response-f)
  (lambda (config)
    (when (findf handler:request->raw? config)
      (error 'on-http/raw "Cannot define both an on-http/raw and on-http in a single big bang"))
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
                
          ;; Enforce the critical region.
          (define in-critical-region
            (let ([a-sema (make-semaphore 1)])
              (lambda (thunk)
                (call-with-semaphore a-sema thunk))))
                                
          
          ;; run-servlet: (world request -> world) (world request -> response) -> void
          (define (run-servlet world-update response-generator)
            (define (handler request)
              (in-critical-region (lambda ()
                                    (local [(define new-world (world-update world request))
                                            (define a-response (response-generator world request))]
                                      (set! world new-world)
                                      a-response))))
            (thread (lambda ()
                      (serve/servlet handler))))
          
          
          ;; handler-start!: handler -> void
          (define (handler-start! a-handler)
            (match a-handler
              [(struct handler:request->raw (world-update-f response-f))
               (run-servlet world-update-f response-f)]
              
              [(struct handler:request->html+css (world-update-f html-response-f css-response-f))
               (run-servlet world-update-f 
                            (html+css-generators->response-generator html-response-f
                                                                     css-response-f))]
              
              [(struct handler:tick (delay world-update-f))
               (thread (lambda ()
                         (let loop ()
                           (sleep delay)
                           (in-critical-region (lambda ()
                                                 (local [(define new-world (world-update-f world))]
                                                   (set! world new-world))))
                           (loop))))]))]
    (for-each handler-start! config-elements)))


;; html+css-generators->response-generator: (world request -> html) (world request -> css-sexpr) -> (world request -> response)
;; Translate the pair of response functions into a single one.
(define (html+css-generators->response-generator html-generator css-generator)
  (lambda (world request)
    (let ([html (html-generator world request)]
          [style-source (css->style-source (css-generator world request))])
      
      (inject-style-source html style-source))))


;; inject-style-source: html string -> html
;; Adds an additional style into the head of the given html.
;;
;; FIXME: inject as CDATA to avoid escaping issues!
;;
(define (inject-style-source html style-source)
  (match html
    [(list 'html 
           (list 'head head-content ...)
           rest ...)
     
     `(html (head ,@head-content
                  (style ,(leave-unescaped style-source)))
            ,@rest)]
    
    [(list 'html 
           (list 'body body-content ...)
           rest ...)
     `(html (head (style ,(leave-unescaped style-source)))
            (body ,@body-content)
            ,@rest)]
    
    [else
     html]))


;; Hacky.  Should we worry about injection attacks?
(define (leave-unescaped content)
  (make-cdata #f #f content))


;; css->style-source: css -> string
(define (css->style-source a-css) 
  (match a-css
    [(list (list id (list names valss ...) ...)
           rest-css ...)
     (string-append (css-clause->string id names valss)
                    (css->style-source rest-css))]
    [(list)
     ""]))


;; stringify: X -> string
(define (stringify val)
  (format "~a" val))


;; id->class-selector: (or symbol string) -> string
(define (id->class-selector an-id)
  (format "#~a" an-id))


;; css-clause->string: id (listof string) (listof (listof string))) -> string
(define (css-clause->string id names valss)
  (let ([key-value-pairs
         (let loop ([names names]
                    [valss valss])
           (cond
             [(empty? names)
              empty]
             [else
              (cons (string-append
                     (format "~a" (first names))
                     " : "
                     (string-join (map (lambda (v) (format "~s" (stringify v))) (first valss)) " "))
                    (loop (rest names) (rest valss)))]))])
    (string-append (id->class-selector id)
                   " { "
                   (string-join key-value-pairs "; ") 
                   " } ")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A small function to exercise the framework.
(define (simple-test-1)
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

;; Another one that lets us try the css model.
(define (simple-test-2)
  (local [(define-struct world (hits seconds))
          
          ;; tick: world -> world
          (define (tick w)
            (make-world (world-hits w)
                        (add1 (world-seconds w))))
          
          ;; update-on-request: world request -> world
          (define (update-on-request w req)
            (make-world (add1 (world-hits w))
                        (world-seconds w)))
          
          ;; draw-html: world request -> html
          (define (draw-html w req)
            `(html (head)
                   (title "Hello World!")
                   (body (h1 "Hello World!")
                         (p ((id "aPara"))
                            "Hello world, I've seen "
                            ,(number->string (world-hits w))
                            ".  "
                            ,(number->string (world-seconds w))
                            " seconds have passed since startup."))))
          
          ;; draw-css: world request -> css
          (define (draw-css w req)
            `(("aPara" ("font-size" "30px"))))]
    
    (big-bang (make-world 0 0)               
              (on-tick 1 tick)
              (on-http update-on-request draw-html draw-css))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-primitive big-bang)
(provide-primitive request-lookup)
(provide-primitive request-has?)

(provide-higher-order-primitive on-http (world-update http-response-generator css-response-generator))
(provide-higher-order-primitive on-http/raw (world-update response-generator))
(provide-higher-order-primitive on-tick (delay handler))