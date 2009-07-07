;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ALL TIMES ARE IN SECONDS, ALL LENGTHS ARE IN METERS, ALL VELOCITIES ARE IN M/S, ALL BEARINGS ARE IN RADIANS
(require "http-world.ss")


;;;; CONSTANTS

;; Time between ticks (seconds)
(define TICK_TIME 10)
;; Time between updates (in TICKS)
(define UPDATE_TICK (/ 20 TICK_TIME))
;; Time between updates (in Seconds)
(define UPDATE_TIME (* TICK_TIME UPDATE_TICK))
;; Maximum timeout time for a person's data (the number of seconds after which someone's data will be left out)
(define PERSON_TIMEOUT 300)
;; Maximum timeout time for a place's data
(define PLACE_TIMEOUT 40)


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
;;;; an unnamed-place has a lattitude (degrees) and a longitude (degrees)
;;;;if asked for a name it will give: "Unknown (<lattitude>, <longitude>)", if asked for a radius it will default to 0.5 miles (but will give the equivalent in meters)
(define-struct unnamed-place (latitude longitude infos))
(define UNNAMED_PLACE_RADIUS 20) ;; The number of meters in the radius of an unnamed place
;; the statusInfo of a place has numberOfPeople, avgSpeed, and prevousTrafficLevel
(define-struct statusInfo (dir-speeds trafficLevel time))
;; a dir-speed consists of a center-bearing,
(define-struct dir-speed (center-bearing numPeople avgSpeed))
;; BEARING_MARGIN is the max number of radians that a velocity can be from the center-bearing of a dir-speed to be a part of that direction
(define BEARING_MARGIN (/ pi 8))


;;;; WORLD

;; a world consists of a list of places, a hash of people, and a number representing the absolute time in seconds
(define-struct world (places people absolute-time))

;; Test Places
(define testPlace1 (make-named-place "Boston" 42.37 -71.05 2 (list (make-statusInfo (list (make-dir-speed 0.2 4 2.7) (make-dir-speed 1.57 1 3.5)) 0 1) (make-statusInfo (list (make-dir-speed 0.2 5 3.0)) 0 0))))
(define testPlace2 (make-unnamed-place 42.38 -71 (list)))
(define testPlace3 (make-unnamed-place 42 -71 (list (make-statusInfo (list (make-dir-speed 3.5 1 5)) 0 1))))


(define initPlaces (list testPlace1 testPlace2 testPlace3))
(define initPeople (list ))

(define initWorld (make-world initPlaces initPeople 0))





;;;; ON TICK
;;;; Removes all expired data
(define (tock w)
    (if (< (mod (world-absolute-time w) UPDATE_TIME) TICK_TIME)
       (make-world (cleanPlaces (world-places w) (- (world-absolute-time w) PLACE_TIMEOUT)) (cleanPeople (world-people w) (- (world-absolute-time w) PERSON_TIMEOUT)) (+ TICK_TIME (world-absolute-time w)))
  (make-world (world-places w) (world-people w) (+ (world-absolute-time w) TICK_TIME))))

;;;; Removing old place data

;; cleanPlaces: (listof place) -> (listof place)
;; removes all old information from places and removes unnamed places from the list if they no longer contain any data
(define (cleanPlaces allPlaces minTime)
  (cond
    [(empty? allPlaces) empty]
    [(named-place? (first allPlaces)) (cons (make-named-place (named-place-name (first allPlaces))
                                                              (named-place-latitude (first allPlaces))
                                                              (named-place-longitude (first allPlaces))
                                                              (named-place-radius (first allPlaces))
                                                              (cleanInfoList (named-place-infos (first allPlaces)) minTime))
                                            (cleanPlaces (rest allPlaces) minTime))]
    [(unnamed-place? (first allPlaces)) (let ((newInfoList (cleanInfoList (place-infos (first allPlaces)) minTime)))
                                          (cond
                                            [(empty? newInfoList) (cleanPlaces (rest allPlaces) minTime)]
                                            [else (cons (make-unnamed-place (unnamed-place-latitude (first allPlaces))
                                                                            (unnamed-place-longitude (first allPlaces))
                                                                            newInfoList)
                                                        (cleanPlaces (rest allPlaces) minTime))]
                                            ))]
    ))

;; removes any status infos with times below minTime
(define (cleanInfoList infoList minTime)
  (cond
    [(empty? infoList) empty]
    [(< (statusInfo-time (first infoList)) minTime) empty]
    [else (cons (first infoList) (cleanInfoList (rest infoList) minTime))]))

;;;; Removing old person data
;; Removes all old information from people and ????? removes people from the list if they no longer contain any data ?????
(define (cleanPeople allPeople minTime)
  (cond
    [(empty? allPeople) empty]
    [else (let ((newDataList (cleanDataList (person-data (first allPeople)) minTime)))
            (cond
              [(empty? newDataList) (cleanPeople (rest allPeople) minTime)] ;; depending on whether or not you want to remove this person, change this line
              [else (cons (make-person (person-name (first allPeople)) newDataList) (cleanPeople (rest allPeople) minTime))]
              ))]
    ))

;; Removes any place-times with times below minTime
(define (cleanDataList dataList minTime)
  (cond
    [(empty? dataList) empty]
    [(< (place-time-time (first dataList)) minTime) empty]
    [else (cons (first dataList) (cleanDataList (rest dataList) minTime))]
    ))


;;;; ON REQUEST
;;;; When a person has pinged, add the new data to the list of people and of places

;; Let us assume (at least for right now that req is a structure containing name, latitude, and longitude
(define (add-person w req)
  (if (and (request-has? req "name")
           (request-has? req "lat")
           (request-has? req "lon")
           (alphabet-and-number? (request-lookup req "name"))
           (number? (string->number (request-lookup req "lat")))
           (number? (string->number (request-lookup req "lon")))) ;; These are the requirements for the parameters which will be sent as requests (they must be usable, present, and convertible
      (let ((aName (request-lookup req "name")) (aLat (string->number (request-lookup req "lat"))) (aLng (string->number (request-lookup req "lon"))))
        (let ((newPersonList (addNewData (world-people w) aName aLat aLng (world-absolute-time w))))
          (if (and (boolean? newPersonList) (false? newPersonList))
              w
              (make-world (addPersonToPlaces (get-person newPersonList aName)
                                             (world-absolute-time w)
                                             (if (contains-location? (world-places w) aLat aLng)
                                                 (world-places w)
                                                 (cons (make-unnamed-place aLat aLng empty)
                                                       (world-places w))))
                          newPersonList
                          (world-absolute-time w)))))
      w))

;; Adds the data at the given time from the given person to the given list of places (the list of places must have at least one place in it which contains the coordinates of the person's most recent location)
(define (addPersonToPlaces aPerson curTime alop)
  (cond
    [(empty? alop) empty]
    [(contains? (first alop) (place-time-latitude (first (person-data aPerson))) (place-time-longitude (first (person-data aPerson))))
     (cons (cond 
             [(named-place? (first alop)) (make-named-place (named-place-name (first alop))
                                                            (place-latitude (first alop))
                                                            (place-longitude (first alop))
                                                            (named-place-radius (first alop))
                                                            (addPersonToInfos aPerson curTime (named-place-infos (first alop))))]
             [(unnamed-place? (first alop)) (make-unnamed-place (place-latitude (first alop))
                                                                (place-longitude (first alop))
                                                                (addPersonToInfos aPerson curTime (place-infos (first alop))))])
           (addPersonToPlaces aPerson curTime (rest alop)))]
    [else (cons (first alop) (addPersonToPlaces aPerson curTime (rest alop)))]))


;; adds the first place-time data from the given person to the given list of places and returns the list of places
(define (addPersonToInfos aPerson curTime aloSI)
  (cond
    [(or (empty? aloSI) ;; if empty or without an up-to-date SI, cons (new first) aloSI
         (not (= (place-time-time (first (person-data aPerson)))
                 (statusInfo-time (first aloSI)))))
     (cons (make-statusInfo (list (make-dir-speed (velocity-dir (place-time-vel (first (person-data aPerson))))
                                                  1
                                                  (velocity-speed (place-time-vel (first (person-data aPerson))))))
                            0
                            curTime)
           aloSI)]
    [else ;; if the person's first place-time has the same time as the first statusinfo, add it there (keeping in mind direction etc)
     (cons (make-statusInfo (addDataToDS (place-time-vel (first (person-data aPerson))) (statusInfo-dir-speeds (first aloSI)) false)
                            (statusInfo-trafficLevel (first aloSI))
                            (statusInfo-time (first aloSI)))
           (rest aloSI))])) 

;; Adds the velocity data from aVel to the list of dir-speed (aloDS), adding a new dir-speed if necessary
(define (addDataToDS aVel aloDS entered)
  (cond
    [(and (= 0 (velocity-speed aVel)) (= -1 (velocity-dir aVel))) (if (empty? aloDS)
                                                                      (list (make-dir-speed -1 1 0))
                                                                      (map (lambda (x) (make-dir-speed (dir-speed-center-bearing x)
                                                                                                       (add1 (dir-speed-numPeople x))
                                                                                                       (/ (+ (* (dir-speed-numPeople x)
                                                                                                                (dir-speed-avgSpeed x))
                                                                                                             (velocity-speed aVel))
                                                                                                          (add1 (dir-speed-numPeople x)))))
                                                                           aloDS))]
    [(and (empty? aloDS) (not entered)) (list (make-dir-speed (velocity-dir aVel) 1 (velocity-speed aVel)))]
    [(and (empty? aloDS) entered) empty]
    [(or (and (< (dir-speed-center-bearing (first aloDS)) BEARING_MARGIN)
              (>= (velocity-dir aVel) (- (+ (* 2 pi) (dir-speed-center-bearing (first aloDS))) BEARING_MARGIN)))
         (and (> (dir-speed-center-bearing (first aloDS)) (- (* 2 pi) BEARING_MARGIN))
              (<= (velocity-dir aVel) (- (+ BEARING_MARGIN (dir-speed-center-bearing (first aloDS))) (* 2 pi))))
         (and (<= (velocity-dir aVel) (dir-speed-center-bearing (first aloDS)))
              (>= (velocity-dir aVel) (dir-speed-center-bearing (first aloDS)))))
     (cons (make-dir-speed (dir-speed-center-bearing (first aloDS))
                           (add1 (dir-speed-numPeople (first aloDS)))
                           (/ (+ (* (dir-speed-numPeople (first aloDS))
                                    (dir-speed-avgSpeed (first aloDS)))
                                 (velocity-speed aVel))
                              (add1 (dir-speed-numPeople (first aloDS)))))
           (addDataToDS aVel (rest aloDS) true))]
    [else (cons (first aloDS) (addDataToDS aVel (rest aloDS) entered))]))

;; Adds the data to the list of people, and returns the list of people (returns false if the person named already has data at the current time)
(define (addNewData alop aName aLat aLng curTime)
  (cond
    [(contains-person? alop aName)
     (if (<= curTime (place-time-time (first (person-data (get-person alop aName)))))
         false
         (cons (make-person aName
                            (cons (make-place-time aLat
                                                   aLng
                                                   (get-velocity (place-time-latitude (first (person-data (get-person alop aName))))
                                                                 (place-time-longitude (first (person-data (get-person alop aName))))
                                                                 aLat
                                                                 aLng     
                                                                 (- curTime
                                                                    (place-time-time (first (person-data (get-person alop aName)))))) curTime)
                                  (person-data (get-person alop aName))))
               (remove-person alop aName)))]
    [else (cons (make-person aName (list (make-place-time aLat
                                                          aLng
                                                          (make-velocity 0 -1)
                                                          curTime)))
                alop)]))

;;;; RESPONSE FUNCTIONS -- THIS IS CURRENTLY VERY UGLY AND WILL NEED TO BE CLEANED UP -- SHOULD I DO THIS OR SHOULD I WAIT TILL DANNY IS FINISHED WITH HIS GOOGLE MAPS IMPLEMENTATION IN SCHEME?

;; The response to someone sending their data
(define (make-response w req)
  `(html ((xmlns "http://www.w3.org/1999/xhtml")
          (xmlns:v "urn:schemas-microsoft-com:vml"))
         (head (meta ((http-equiv "content-type")
                      (content "text/html; charset=utf-8")))
               (title ,(string-append "Google Maps-Scheme Mashup -- " (number->string (world-absolute-time w)) " seconds"))
               
               (script ((src "http://maps.google.com/maps?file=api&amp;v=2&amp;sensor=false&amp;key=ABQIAAAA5M6PzUJFRJPgy3QvsbR7hxQw4RzD19GyORoXCg-d1S3AgrtlrBR_aFXmP8VRkolg6qIvOT68quvYLQ")) "")
               (script ((type "text/javascript"))
                       ,(string-append "
    function initialize() {
      if (GBrowserIsCompatible()) {
        var map = new GMap2(document.getElementById(\"map_canvas\"));
        map.setCenter(new GLatLng(42.37, -71.05), 8);
        map.addControl(new GLargeMapControl());
        "
                                       (makeAllPlaceMarkers "map" (world-places w))
                                       
                                       "
      }
    }"
                                       )
                       )
               )
         ,(append (list 'body 
                        `((onload "initialize()") (onunload "GUnload()") (style "font-family: Arial;border: 0 none;"))
                        `(div ((id "map_canvas") (style "width: 480px; height: 320px")) "")
                        `(h1 "People:"))
                  (foldl (lambda (nextItem already)
                           (append (foldr (lambda (nI al)
                                            (append al
                                                    (list `(p ,(string-append "Time: "
                                                                              (number->string (place-time-time nI))
                                                                              ", Lat: "
                                                                              (number->string (exact->inexact (place-time-latitude nI)))
                                                                              ", Lon: "
                                                                              (number->string (exact->inexact (place-time-longitude nI))) 
                                                                              ", "
                                                                              (velocity->string (place-time-vel nI)))))))
                                          (list `(h2 ,(string-append "Name: " (person-name nextItem))))
                                          (person-data nextItem)) already))
                         empty
                         (world-people w))
                  (list `(h1 "Places:"))
                  (foldl (lambda (nextPlace already)
                           (append (foldr (lambda (nI al)
                                            (append al
                                                    (list `(p ,(string-append "Bearing: "
                                                                              (makeNumPrintable (dir-speed-center-bearing nI) -6)
                                                                              ", # Of People: "
                                                                              (makeNumPrintable (dir-speed-numPeople nI) 0)
                                                                              ", avgSpeed: "
                                                                              (makeNumPrintable (dir-speed-avgSpeed nI) -6))))))
                                          (if (empty? (place-infos nextPlace))
                                              (list `(h2 ,(string-append (place->string nextPlace)))
                                                    `(p "Empty..."))
                                              (list `(h2 ,(string-append (place->string nextPlace)
                                                                         ", Time:"
                                                                         (number->string (statusInfo-time (first (place-infos nextPlace))))))))
                                          (if (empty? (place-infos nextPlace))
                                              empty 
                                              (statusInfo-dir-speeds (first (place-infos nextPlace))))) already))
                         empty
                         (world-places w))
                  )
         )
  )

;; Temp marker make -- each call to this must have a different markerName
(define (temp-make-marker mapName markerName latlng infoText)
  (string-append
   "infoWindowOptions"
   markerName
   " = { maxWidth: 200};
        var point"
   markerName
   " = new GLatLng("
   (number->string (exact->inexact (posn-x latlng)))
   ","
   (number->string (exact->inexact (posn-y latlng)))
   ");
        var aMarker"
   markerName
   " = new GMarker( point"
   markerName
   ");
        GEvent.addListener( aMarker"
   markerName
   ", \"click\", function() {
          aMarker"
   markerName
   ".openInfoWindow(document.createTextNode(\"---"
   markerName
   "---"
   infoText
   "\"), infoWindowOptions"
   markerName
   ");
        });
        map.addOverlay(aMarker"
   markerName
   ");
"))

;; Make a series of markers from a series of places
(define (makeAllPlaceMarkers mapName alops)
  (cond
    [(empty? alops) ""]
    [else (string-append (temp-make-marker mapName
                                           (place->string (first alops))
                                           (make-posn (place-latitude(first alops))
                                                      (place-longitude (first alops)))
                                           (if (empty? (place-infos (first alops)))
                                               ""
                                               (string-append "Time: "
                                                              (makeNumPrintable (statusInfo-time (first (place-infos (first alops)))) 0)
                                                              (foldr (lambda (x al)
                                                                       (string-append al
                                                                                      (infos->string x)))
                                                                     ""
                                                                     (statusInfo-dir-speeds (first (place-infos (first alops))))  ;; this if statement is a bit of a kludge -- try to make it prettier
                                                                     ))))
                         (makeAllPlaceMarkers mapName (rest alops)))]))

(define (infos->string aInfo)
  (string-append "Bearing: " (makeNumPrintable (dir-speed-center-bearing aInfo) -3)
                 ", Number of People: " (number->string (dir-speed-numPeople aInfo))
                 ", Average Speed: " (makeNumPrintable (dir-speed-avgSpeed aInfo) 0)))

;;;; ALL HELPER FUNCTIONS

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

;; Returns true iff alop contains a place with lat and lng within the radius of the place
(define (contains-location? alop lat lng)
  (cond
    [(empty? alop) false]
    [(contains? (first alop) lat lng) true]
    [else (contains-location? (rest alop) lat lng)]))



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
    [(and (= latA latB) (= lonA lonB)) -1]
    [else (mod (+ pi
                  (atan (* (sin (- (deg->rad lonB)
                                   (deg->rad lonA)))
                           (cos (deg->rad latB)))
                        (- (* (cos (deg->rad latA))
                              (sin (deg->rad latB)))
                           (* (sin (deg->rad latA))
                              (cos (deg->rad latB))
                              (cos (- (deg->rad lonB)
                                      (deg->rad lonA)))))))
               (* 2 pi))]))


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
  (string-append (cond
                   [(named-place? a-place)
                    (string-append (named-place-name a-place) "_")]
                   [(unnamed-place? a-place)
                    "Unknown_"])
                 (makeNumPrintable (place-latitude a-place) -6)
                 "_"
                 (makeNumPrintable (place-longitude a-place) -6)
                 "_"))

;; place-radius: place -> number
;; Given a place, returns its radius.
(define (place-radius a-place)
  (cond
    [(named-place? a-place)
     (named-place-radius a-place)]
    [(unnamed-place? a-place)
     UNNAMED_PLACE_RADIUS]))

1
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

;; determines if the given place contains the given lat and lng
(define (contains? aPlace aLat aLng)
  (>= (place-radius aPlace) (distance aLat
                                      aLng
                                      (place-latitude aPlace)
                                      (place-longitude aPlace))))

;;;; OTHER HELPER FUNCTIONS

;; mod: number(a) number(b) -> number
;; returns mod_b(a) (will be [0, b) even if a is negative -- b must be positive)
(define (mod a b)
  (cond
    [(< a 0) (mod (+ a b) b)]
    [(< a b) a]
    [else (mod (- a b) b)]))

;; Determines if the string is made of only a-z, A-Z, and 0-9
(define (alphabet-and-number? aString)
  (cond
    [(= (string-length aString) 0) true]
    [(not (or (char-alphabetic? (string-ref aString 0)) (char-numeric? (string-ref aString 0)))) false]
    [else (alphabet-and-number? (substring aString 1))]))

;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))

;; deg->rad: number -> number
(define (deg->rad aDeg)
  (/ (* aDeg 2 pi) 360))

;; Turns a velocity into a meaningful string
(define (velocity->string aVel)
  (string-append "Speed: " (makeNumPrintable (velocity-speed aVel) -3) ", Dir: " (makeNumPrintable (velocity-dir aVel) -3)))

;; for place-num = 0 will round to the nearest integer, for place-num = 1 will round to the nearest 10 (works for negative integers
(define (roundToPlace aNum placeNum)
  (cond
    [(= 0 placeNum) (round aNum)]
    [(< 0 placeNum) (* 10.0 (roundToPlace (/ aNum 10.0) (sub1 placeNum)))]
    [(> 0 placeNum) (/ (roundToPlace (* aNum 10.0) (add1 placeNum)) 10.0)]))

;; replaces the given substring with the replacement wherever the substring appears in aString
(define (replace-substring aString aSubString replacement)
  (cond
    [(equal? (string-length aString) 0) ""]
    [(equal? aSubString (substring aString 0 (string-length aSubString))) (string-append replacement (replace-substring (substring aString (string-length aSubString)) aSubString replacement))]
    [else (string-append (substring aString 0 1) (replace-substring (substring aString 1) aSubString replacement))]))

;; Makes a num printable in javascript
(define (makeNumPrintable aNum decPlacesToRound)
  (string-append (if (negative? aNum) "neg" "") (replace-substring (number->string (exact->inexact (roundToPlace (abs aNum) decPlacesToRound))) "." "D")))

;;;; BIG-BANG ETC

(big-bang initWorld (on-tick TICK_TIME tock)
          (on-http add-person make-response)) ;; on-tick still needs to be written in