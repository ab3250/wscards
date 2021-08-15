(use-modules
  (srfi srfi-42)
  (srfi srfi-1)
  (json json)
  (web request)
  (web response)
  (sxml simple)
  (web http)
  (web server)
  (web uri)
  (ice-9 rdelim)
  (rnrs bytevectors)
  (ice-9 binary-ports)
  (oop goops)
  (srfi srfi-18)
  (ice-9 iconv)
  (ice-9 receive)
  (ice-9 regex)
  (web socket server))




(define fill-array (lambda(l1s l1e l2s l2e)
  (list-ec (: s l1s l1e) (: f l2s l2e)  (string-append (padn s 2) (padn f 2) ))))

(define fill-array-box (lambda(stop)
  (list-ec (: j stop) (if (< j (/ stop 2)) "0" "1" ))))

(set! *random-state* (random-state-from-platform))

(define deck (fill-array 0 4 0 13))

(define deck2 (fill-array-box 80000))

(display (scm->json-string "((name . \"A-Team\") (lead (name . \"Hannibal\") (id . 321)) (devs . #(((name . \"B.A.\") (id . 7)) ((name . \"Murdock\") (id . 13))))))"))

(define (handler data) 
(cond 
    ((string=? data "button1") (scm->json-string (knuth-shuffle deck)))
    ((string=? data "button2") (scm->json-string deck))
    ((string=? data "button3") (scm->json-string deck2))
    ((string=? data "button4") (scm->json-string (knuth-shuffle deck2)))
    (else #f)))
   







;(map (lambda (n) (display n)(newline)) (fill-array 0 4 0 4 '()))
;(display   (string-pad (number->string 3) 2 #\0))
; (define fill-array(lambda (start1 end1 start2 end2 arry)
;   (let loop1 ((index1 start1)(arry1 arry))
;     (if (< index1 end1)
;       (let loop2 ((index2 start2)(arry2 arry1))
;         (if(< index2 end2)
;           (loop2 (+ index2 1)(append arry2 (list index1 index2)))
;           (loop1 (+ index1 1) arry2)))
;       arry1))))
; (define fill-array(lambda (start1 end1 start2 end2 arry)
;   (let loop1 ((index1 start1)(arry1 arry))
;     (if (< index1 end1)
;       (let loop2 ((index2 start2)(arry2 arry1))
;         (if(< index2 end2)
;           (loop2 (+ index2 1)(append arry2 (list(string-append (padn index1 2) (padn index2 2)))))
;           (loop1 (+ index1 1) arry2)))
;       arry1))))
;
;{ "data": [2, 4, 5] }