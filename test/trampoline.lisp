; builds a list from a head and tail
(defn cons (a1 a2)
  ([a1].concat a2))

; coerces into an array, welcome to JS
; mostly used for arguments
(defn to-array (coll)
  (Array.prototype.slice.call coll))

; gets the length of a collection
(defn len (coll)
  coll.length)

; list application of function arguments
(defn app (f args)
  (f.apply null args))

; builds a list
(defn list ()
  (to-array arguments))

; gets the first element of a collection
(defn head (l)
  l[0])

; gets the tail of a collection
(defn tail (l)
  (from l 1))

; slices a collection from start to an end
(defn slice (l start end)
  (l.slice start end))

; slices a collection from a start
(defn from (l start)
  (slice l start (len l)))

; gets the item and the
(defn at (l i)
  (cond
    (empty l)
      (err "cannot at from empty list")

    (== i 0)
      (head l)

    else 
      (at (tail l) (- i 1))))

; checks if the list is empty
(defn empty (l)
  (== (len l) 0))

; construct a trampoline call simply
(defn callCons (f args)
  (list "trampoline-call" f args))

; convenience constructor to go to the next function
(defn callT (f)
  (callCons f (tail (to-array arguments))))

; constructor to return
(defn valT (v)
  (list "trampoline-val" v))

; checks if a trampoline is a call
(defn is-call (trampoline)
  (== (head trampoline) "trampoline-call"))

; runs a trampoline
; takes a function to start and the values to start
(defn runT (f start)
  (do
    ; start out with a call
    (def trampoline (callCons f start))
    (while (is-call trampoline)
      (set! trampoline (app (at trampoline 1) (at trampoline 2))))
    ; return
    (at trampoline 1)))

(defn inc (n acc)
  (if (== n 0)
    (valT acc)
    (do
      (callT inc (- n 1) (+ acc n)))))

;(console.log (callT "fac" 3))
(console.log (runT inc (list (Math.pow 10 5) 0)))