(import
    (chibi time)    
    (websocket)
    (only (srfi 130) string-pad)
    ;(schemepunk json)
    (json)
    ;(chibi string)
    (srfi 1)
    (srfi 27))

(define for-acc (lambda (start end func)
  (let loop ((index start)
             (acc '()))
      (if (> index end)
                      acc   
                      (loop (+ index 1) (func index end acc))))))

(define (random-source-make-integers rs)
  (lambda (n) (%random-integer rs n)))

;;;;
(define-syntax nloop*
  ;; Nested numerical loop
  (syntax-rules ()
    ((_ () form ...)
     (begin form ...
            (values)))
    ((_ ((variable lower-inclusive upper-exclusive) more ...) form ...)
     (let loop ((variable lower-inclusive))
       (if (< variable upper-exclusive)
           (begin
             (nloop* (more ...) form ...)
             (loop (+ variable 1)))
           (values))))
    ((_ ((variable start-inclusive end-exclusive step) more ...) form ...)
     (let ((cmp? (if (>= step 0) < >)))
       (let loop ((variable start-inclusive))
         (if (cmp? variable end-exclusive)
             (begin
               (nloop* (more ...) form ...)
               (loop (+ variable step)))
             (values)))))))

(define (knuth-shuffle lst-org)  
  (let loop ((count (length lst-org)) (lst lst-org))      
    (if (zero? count)
    	lst
	(let*   ((j (random-integer count))
		 (new-count (- count 1))
	         (tmp (list-ref lst new-count))
	         (lst2 (list-set lst new-count (list-ref lst j)))
	         (lst3 (list-set lst2 j tmp)))	         
	         (loop new-count lst3)))))

; (define-syntax for
;   (syntax-rules ()
;     ((_ (i from to) b1 ...)
;      (let loop((i from))
;        (when (< i to)
; 	 b1 ...
; 	 (loop (+ 1 i)))))))

(define (list-set lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-set (cdr lst) (- idx 1) val))))

(define padn (lambda(x y)
  (string-pad (number->string x) y #\0)))

 
;;;;

(define fill-array (lambda(l1s l1e l2s l2e)
  (nloop* ((i l1s l1e)
    (j l2s l2e))
    (string-append (padn i 2) (padn j 2) ))))

(define (func x end acc)
  (cons (if (< (- x 1)(/ end 2)) "0" "1" ) acc))

(define deck2 (for-acc 1 3200 func))
  
  

(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

(define fill-array-box (lambda(stop)
  (list-ec (: j stop) (if (< j (/ stop 2)) "0" "1" ))))

(define deck (list "0000" "0001" "0002" "0003" "0004" "0005" "0006" "0007" "0008" "0009" "0010" "0011" "0012" "0100" "0101" "0102" "0103" "0104" "0105" "0106" "0107" "0108" "0109" "0110" "0111" "0112" "0200" "0201" "0202" "0203" "0204" "0205" "0206" "0207" "0208" "0209" "0210" "0211" "0212" "0300" "0301" "0302" "0303" "0304" "0305" "0306" "0307" "0308" "0309" "0310" "0311" "0312"))



;(define deck (fill-array 0 4 0 13))

;(display deck2)


;(display (json->string (knuth-shuffle deck2)))
;(display (json->string "((name . \"A-Team\") (lead (name . \"Hannibal\") (id . 321)) (devs . #(((name . \"B.A.\") (id . 7)) ((name . \"Murdock\") (id . 13))))))"))
;(define deck2 (fill-array-box 80000))

;;;;

;;;;



(define gblFd 1)
(define port 8080)
(define nonblocking 1)

(define (init fp)   
  (ws_socket fp port nonblocking))

(define (delay sec)
    (define start (current-seconds))
    (let timeloop ()    
        (if ( < (- (current-seconds) start) sec) (timeloop))))

(define (onclose fd)
   (display (list 'closed fd))
   (newline))

(define (onopen fd)
    (set! gblFd fd)
    (display (list 'opened fd))   
    (newline))

(define (ws_send_txt msg)
    (ws_sendframe_txt gblFd msg #f)
    (display (list 'frame 'sent 'client gblFd))
    (newline))

(define (onmessage fd msg size type)    
    (display (list 'frame 'recieved 'client fd))
    (newline)) ;echo

(define (main) 
    (let loop ((count 0))                
        (ws_send_txt (json->string deck))
        (delay 2)              
        (ws_send_txt (json->string (knuth-shuffle deck)))
        (delay 3)


   (loop (+ count 1))))