(import
    (chibi time)  
    (scheme base)
    (scheme red)  
    (websocket)
   (only (srfi 130) string-pad)
   (schemepunk json)
   (srfi 1)
   (srfi 27)
   )

(define for-acc (lambda (start end func)
  (let loop ((index start)
             (acc '()))
      (if (> index end)
                      acc   
                      (loop (+ index 1) (func index end acc))))))


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

(define (func x end acc)
  (cons (if (< (- x 1)(/ end 2)) "0" "1" ) acc))

(define deck2 (for-acc 1 3200 func))

(define deck (list "0000" "0001" "0002" "0003" "0004" "0005" "0006" "0007" "0008" "0009" "0010" "0011" "0012" "0100" "0101" "0102" "0103" "0104" "0105" "0106" "0107" "0108" "0109" "0110" "0111" "0112" "0200" "0201" "0202" "0203" "0204" "0205" "0206" "0207" "0208" "0209" "0210" "0211" "0212" "0300" "0301" "0302" "0303" "0304" "0305" "0306" "0307" "0308" "0309" "0310" "0311" "0312"))

; ;;;;

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
    (newline)
    (ws_send_txt msg)) ;echo

(define (main args)
    (ws_start)
    (let loop ((count 0))   
        (delay 3)                
        (ws_send_txt (json->string(knuth-shuffle deck2)))
        
   (loop (+ count 1))))