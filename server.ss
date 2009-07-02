;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; in moby-scheme/src/test there exists kathi-finder, parse-google-maps-places, and grocery-shopper which all deal with similar principles to this
(require "http-world.ss")


;;;; CONSTANTS

;; Time between ticks (seconds)
(define TICK_TIME 1)
;; Time between updates (in TICKS)
(define UPDATE_TICK 60)
;; Time between updates (in Seconds)
(define UPDATE_TIME (* TICK_TIME UPDATE_TICK))
;; Maximum timeout time for a person's data (the number of seconds after which someone's data will be left out)
(define PERSON_TIMEOUT 300)
;; Maximum timeout time for a place's data
(define PLACE_TIMEOUT 600)


;;;; STRUCTURES

;; a person has a "name" (not sure what type that will be yet -- could be phone #, could be automatically assigned), data (a list of places-times of length no greater than max-data-entries), and a last-updated time
(define-struct person (name data))
;; A place-time consists of latitude, longitude, vel (of type velocity), and a time (an integer indicating which tick the place was recieved after)
(define-struct place-time (latitude longitude vel time))
;; a velocity is defined in terms of speed (in m/s) and direction (bearing in degrees)
(define-struct velocity (speed dir))

;; a place can be either a named-place or an unnamed-place:
;;;; a named-place has a name (String), lattitude (degrees), longitude (degrees), radius (meters)
(define-struct named-place (name latitude longitude radius infos))
;;;; an unnamed-place has a lattitude (degrees) and a longitude (degrees) -- if asked for a name it will give: "Unknown (<lattitude>, <longitude>)", if asked for a radius it will default to 0.5 miles (but will give the equivalent in meters)
(define-struct unnamed-place (latitude longitude infos))
;; the statusInfo of a place has numberOfPeople, avgSpeed, and prevousTrafficLevel
(define-struct statusInfo (dir-speeds trafficLevel))
;; a dir-speed consists of a center-bearing,
(define-struct dir-speed (center-bearing numPeople avgSpeed time))
;; BEARING_MARGIN is the max number of radians that a velocity can be from the center-bearing of a dir-speed to be a part of that direction
(define BEARING_MARGIN (/ pi 8))


;;;; WORLD

;; a world consists of a list of places, a hash of people, and a number representing the absolute time in seconds
(define-struct world (places people absolute-time))

(define initPlaces (list ))
(define initPeople (list ))

(define initWorld (make-world initPlaces initPeople 0))





;;;; ALL UPDATES

#;(define (update-all w)
  (if (< (mod (world-absolute-time w) UPDATE_TIME) TICK_TIME)
      (let ((newPeople (update-people w)))
        (make-world (update-places (world-places w) newPeople (world-absolute-time w)) newPeople (+ TICK_TIME (world-absolute-time w))))
      (make-world (world-places w) (world-people w) (+ (world-absolute-time w) TICK_TIME)))) ;; update-people and update-places still need to be made


;;;; Update Person-related-functions

;; Let us assume (at least for right now that req is a structure containing name, latitude, and longitude
(define (update-person w req)
  (if (and (request-has? req "name") (request-has? req "lat") (request-has? req "lon"))
      (let ((lat (string->number (request-lookup req "lat"))) (lon (string->number (request-lookup req "lon"))) (aName (request-lookup req "name")))
        (make-world (world-places w)
                    (if (and (contains-person? (world-people w) aName)
                             (not (empty? (person-data (get-person (world-people w) aName)))))
                        (cons (make-person aName
                                           (cons (make-place-time lat
                                                                  lon
                                                                  (get-velocity lat
                                                                                lon
                                                                                (place-time-latitude (first (person-data (get-person (world-people w) aName))))
                                                                                (place-time-longitude (first (person-data (get-person (world-people w) aName))))
                                                                                (- (world-absolute-time w) (place-time-time (first (person-data (get-person (world-people w) aName))))))
                                                                  (world-absolute-time w))
                                                 (person-data (get-person (world-people w)
                                                                          aName))))
                              (remove-person (world-people w) aName))
                        (cons (make-person aName
                                           (list (make-place-time lat
                                                                  lon
                                                                  (make-velocity -1 -1) ;; should this be null or some other value?
                                                                  (world-absolute-time w))))
                              (world-people w)))
                    (add1 (world-absolute-time w)))) ;; THIS SHOULD BE CHANGED BACK TO NOT ADDING ONE IN NON-TESTING IMPLEMENTATIONS
      w))

;; Cleans up old data
(define (update-people w)
  (filter (lambda (x) (not (empty? (person-data x)))) ;; right now this removes a person if they have no data -- this can be changed if necessary
          (map (lambda (aPerson)
                 (removeOldPeopleData (person-data aPerson)
                                      (- (world-absolute-time w) PERSON_TIMEOUT)))
               (world-people w))))

;; Eliminates old entries for people given a list of data (ones with time < minTime)
(define (removeOldPeopleData alod minTime)
  (cond
    [(empty? alod) empty]
    [(< (place-time-time (first alod)) minTime) empty]
    [else (cons (first alod) (removeOldPeopleData (rest alod) minTime))]))


;;;; UPDATE PLACES
#;(
;; Updates place data
(define (update-places oldPlaces newPeople curTime)
  (foldl (lambda (y z) (cleanPlaces y z (- curTime PLACE_TIMEOUT)))
         empty
         (map (lambda (x) (addPeopleToPlace newPeople x))
              oldPlaces)))

;; cleanPlaces: (listof place) -> (listof place)
;; removes all old information from places and removes unnamed places from the list if they no longer contain any data
(define (cleanPlaces aPlace already minTime)
  (let ((newPlace (removeOldPlaceData (statusInfo-dir-speeds (place-infos aPlace)) minTime)))
    (cond
      [(empty? (place-infos aPlace)) already]
      [else (cons newPlace already)])))
              

;; Eliminates old entries for places given a list of data (ones with time < minTime)
(define (removeOldPlaceData alod minTime)
  (cond
    [(empty? alod) empty]
    [(< (dir-speed-time (first alod)) minTime) empty]
    [else (cons (first alod) (removeOldPlaceData (rest alod) minTime))]))

;; Adds to the given place the relevant set of people from the given list of people (returns the list)
(define (addPeopleToPlace newPeople aPlace)
  (let ((newInfos (cons (foldl addPersonToPlace
                               empty
                               newPeople)
                        (place-infos aPlace))))
    (cond
      [(named-place? aPlace)]
      [else ])))

;; Adds to the given place the relevant data from the given person (returns the place)
(define (addPersonToPlace aPerson aPlace)
  (cond
    [()]
    []))
)
;;;; RESPONSE FUNCTIONS

;; The response to someone sending their data
(define (make-response w req)
  `(html ((xmlns "http://www.w3.org/1999/xhtml")
          (xmlns:v "urn:schemas-microsoft-com:vml"))
         (head (meta ((http-equiv "content-type")
                      (content "text/html; charset=utf-8")))
               (title "Google Maps-Scheme Mashup")
               (script ((src "http://maps.google.com/maps?file=api&amp;v=2&amp;sensor=false&amp;key=ABQIAAAA5M6PzUJFRJPgy3QvsbR7hxQw4RzD19GyORoXCg-d1S3AgrtlrBR_aFXmP8VRkolg6qIvOT68quvYLQ")) "")
               (script ((type "text/javascript"))
                       "
    function initialize() {
      if (GBrowserIsCompatible()) {
        var map = new GMap2(document.getElementById(\"map_canvas\"));
        map.setCenter(new GLatLng(42.37, -71.05), 13);
        map.addControl(new GSmallMapControl());
        // Add 10 markers to the map at random locations
        var bounds = map.getBounds();
        var southWest = bounds.getSouthWest();
        var northEast = bounds.getNorthEast();
        var lngSpan = northEast.lng() - southWest.lng();
        var latSpan = northEast.lat() - southWest.lat();
        var point = new GLatLng(southWest.lat() + latSpan * Math.random(),
                                southWest.lng() + lngSpan * Math.random());
        var aMarker = new GMarker(point);
        GEvent.addListener( aMarker, \"click\", function() {
          aMarker.openInfoWindow(document.createTextNode(\"A worked!!!\"));
        });
        map.addOverlay(aMarker);
        var pointB = new GLatLng(southWest.lat() + latSpan * Math.random(),
                                 southWest.lng() + lngSpan * Math.random());
        var aMarkerB = new GMarker(pointB);
        GEvent.addListener( aMarkerB, \"click\", function() {
          aMarkerB.openInfoWindow(document.createTextNode(\"Part B worked!!!\"));
        });
        map.addOverlay(aMarkerB);
      }
    }"
                       )
               )
         (body ((onload "initialize()") (onunload "GUnload()") (style "font-family: Arial;border: 0 none;"))
               (div ((id "map_canvas") (style "width: 480px; height: 320px")) "")
               )
         )
  )
  
  
  
  
  #;  `(html ,(cons 'body
                  (foldl (lambda (nextItem already)
                           (append (foldr (lambda (nI al)
                                            (append al (list `(p ,(string-append "Time: " (number->string (place-time-time nI))) ,"\t\t" ,(string-append "Lat: " (number->string (exact->inexact(place-time-latitude nI)))) ,"\t\t" ,(string-append "Lon: " (number->string (exact->inexact(place-time-longitude nI)))) ,"\t\t" ,(velocity->string (place-time-vel nI))))))
                                          (list `(p ,(string-append "Name: " (person-name nextItem))))
                                          (person-data nextItem)) already))
                         (list)
                         (world-people w))))

;; EXAMPLE:
#; `(html (body (p "Hello world")
                (p "This should be another paragraph"
                   "Hello")))


;(define (constuct-marker  

;; Update all info (updating the peoples info will be done before everything else)



;;;; PEOPLE LISTS

;; returns true iff alop contains a person with name field equal to aName
(define (contains-person? alop aName)
  (cond
    [(empty? alop) false]
    [(equal? (person-name (first alop)) aName) true]
    [else (contains-person? (rest alop) aName)]))

;; Removes the first instance of the person with name aName in the list of people alop
(define (remove-person alop aName)
  (cond
    [(empty? alop) false]
    [(equal? (person-name (first alop)) aName) (rest alop)]
    [else (cons (first alop) (remove-person (rest alop) aName))]))

;; Returns a person with name equal to aName in alop (an exception will be thrown if the person does not exist in the list)
(define (get-person alop aName)
  (cond
    [(equal? (person-name (first alop)) aName) (first alop)]
    [else (get-person (rest alop) aName)]))



;;;; VELOCITY RELATED FUNCTIONS

;; gives the velocity given two sets of latitude and longitude
(define (get-velocity a b c d timeDiff)
  (make-velocity (/ (distance a b c d) timeDiff) (direction a b c d)))

;; distance: num num num num -> number
;; Given two places on a globe, return the shortest distance between them in meters (uses spherical geometry)
(define (distance latA lonA latB lonB)
  (* 6378000 
     (* 2
        (asin (min 1
                   (sqrt (+ (expt (sin (/ (- (deg->rad latA) (deg->rad latB)) 2)) 2)
                            (* (cos (deg->rad latA))
                               (cos (deg->rad latB))
                               (expt (sin (/ (- (deg->rad lonA) (deg->rad lonB)) 2)) 2)))))))))

;; direction: num num num num -> number
;; Given two places on a globe, return the bearing of the shortest distance between them in meters (uses spherical geometry)
(define (direction latA lonA latB lonB)
  (cond 
    [(= (cos (deg->rad latA)) 0) (if (latA > 0) pi (* 2 pi))]
    [else (mod (+ pi (atan (* (sin (- (deg->rad lonB) (deg->rad lonA))) (cos (deg->rad latB))) (- (* (cos (deg->rad latA)) (sin (deg->rad latB))) (* (sin (deg->rad latA)) (cos (deg->rad latB)) (cos (- (deg->rad lonB) (deg->rad lonA))))))) (* 2 pi))]))

;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))

;; deg->rad: number -> number
(define (deg->rad aDeg)
  (/ (* aDeg 2 pi) 360))

;; Turns a velocity into a meaningful string
(define (velocity->string aVel)
  (string-append "Speed: " (number->string (velocity-speed aVel)) ", Dir: " (number->string (velocity-dir aVel))))


;;;; PLACE HELPER FUNCTIONS

;; place-infos: place -> statusInfos
(define (place-infos a-place)
  (cond
    [(named-place? a-place)
     (named-place-infos a-place)]
    [(unnamed-place? a-place)
     (unnamed-place-infos a-place)]))

;; place->string: place -> string
(define (place->string a-place)
  (cond
    [(named-place? a-place)
     (named-place-name a-place)]
    [(unnamed-place? a-place)
     (string-append "Unknown (" 
                    (number->string (unnamed-place-latitude a-place))
                    ", "
                    (number->string (unnamed-place-longitude a-place))
                    ")")]))

;; place-radius: place -> number
;; Given a place, returns its radius.
(define (place-radius a-place)
  (cond
    [(named-place? a-place)
     (named-place-radius a-place)]
    [(unnamed-place? a-place)
     (mile->meter 0.5)]))


;; place-latitude: place -> number
(define (place-latitude a-place)
  (cond
    [(named-place? a-place)
     (named-place-latitude a-place)]
    [(unnamed-place? a-place)
     (unnamed-place-latitude a-place)]))


;; place-longitude: place -> number
(define (place-longitude a-place)
  (cond
    [(named-place? a-place)
     (named-place-longitude a-place)]
    [(unnamed-place? a-place)
     (unnamed-place-longitude a-place)]))

;; place-matches?: place number number -> boolean
(define (place-matches? a-place a-lat a-long)
  (<= (distance a-lat 
                a-long 
                (place-latitude a-place) 
                (place-longitude a-place))
      (place-radius a-place)))


;;;; OTHER HELPER FUNCTIONS

;; mod: number(a) number(b) -> number
;; returns mod_b(a) (will be [0, b) even if a is negative -- b must be positive)
(define (mod a b)
  (cond
    [(< a 0) (mod (+ a b) b)]
    [(< a b) a]
    [else (mod (- a b) b)]))


;;;; BIG-BANG ETC

(big-bang initWorld ;;(on-tick TICK_TIME update-all)
          (on-http update-person make-response)) ;; on-tick still needs to be written in