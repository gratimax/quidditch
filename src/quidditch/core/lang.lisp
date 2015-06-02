(ns quidditch/core/lang)

; builds a list
(defn list ()
  (to-array arguments))

; checks if the list is empty
(defn empty (l)
  (== (len l) 0))

; gets the first elements of an array
(defn init (l)
  (slice l 0 (- (len l) 1)))

; gets the last element of an array
(defn last (l)
  (at l (- (len l) 1)))

; puts the element at the end of the list
(defn push (l elem)
  (conc l (list elem)))

; checks for existence, whether an item is inside a collection
(defn has (l item)
  (cond
    (empty l) false
    (== (head l) item) true
    else (has (tail l) item)))

; gets the item and the
(defn at (l i)
  (cond
    (empty l)
      (err "cannot at from empty list")

    (== i 0)
      (head l)

    else 
      (at (tail l) (- i 1))))

; slices a collection from a start
(defn from (l start)
  (slice l start (len l)))

; gets the tail of a collection
(defn tail (l)
  (from l 1))

; inclusive range from 0
(defn irange (end)
  (if (== end 0)
    (list 0)
    (push (irange (- end 1)) end)))

; exclusive range from 0
(defn range (end)
  (init (irange end)))

; zips two same-length collections into one of lists
(defn zip (coll1 coll2) 
  (if (== (len coll1) (len coll2))
    (map (range (len coll1)) (fn (i) (list (at coll1 i) (at coll2 i))))
    (err "collections must be same size")))

; takes a number of elements
(defn take (i coll)
  (if (== i 0)
    nil
    (push (take (- i 1) coll) (at coll i))))

; groups every two elements together
(defn group2 (coll)
  (if (empty coll)
    nil
    (cons (list (head coll) (at coll 1)) (group2 (from coll 2)))))

; reverses the collection
(defn reverse (coll)
  (fold coll (fn (acc cur)
    (cons cur acc)) nil))

; composes two functions together
(defn comp (a b)
  (fn () 
    (a (app b (to-array arguments)))))

; replace except using split
(defn resplit (s mat val)
  (join (split s mat) val))

; builds a string
(defn str ()
  (join (to-array arguments) ""))

; checks if an object is an array
(defn is-array (obj)
  (== (obj-str obj) "[object Array]"))

; checks if an object is a string
(defn is-string (obj)
  (== (obj-str obj) "[object String]"))

(defn debug (str val)
  (do
    (log str)
    val))