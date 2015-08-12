
; try/catch, returns an either
(defn try-call (f)
  (do
    (js "var result")
    (js "try { result = ['right', f()]; } catch (e) { result = ['left', e]; }")
    (js "result")))

; calls a js function
(defn js-call (obj f & args)
  (js "obj[f].apply(obj, args)"))

; creates a mutable js object
(defn new-object ()
  (js "{}"))

; gets the key of an object
(defn obj-get (obj key)
  (js "obj[key]"))

; same function, but when used in js interop context
(def js-get obj-get)

; sets the key of an object to the given value
(defn obj-set! (obj key value)
  (do
    (js "obj[key] = value")
    obj))

; checks if the obj has a key
(defn has-key (obj key)
  (&& (js "obj[key]") (js-call obj "hasOwnProperty" key)))

; builds a list from a head and tail
(defn cons (a1 a2)
  (js-call (js "[a1]") "concat" a2))

; concats the given lists, second after first
(defn conc (a1 a2)
  (js-call a1 "concat" a2))

; gets the length of a collection
(defn len (coll)
  (js-get coll "length"))

; gets the index of an item in a collection
(defn index-of (coll i)
  (js-call coll "indexOf" i))

; replaces something in a string
(defn replace (s mat val)
  (js-call s "replace" mat val))

; gets the first element of a collection
(defn head (l)
  (js "l[0]"))

; gets the object string for an object
(defn obj-str (obj)
  (js "Object.prototype.toString.call(obj)"))

; checks if the string matches the given regex
(defn matches (stri regex)
  (js-call stri "match" regex))

; slices a collection from start to an end
(defn slice (l start end)
  (js-call l "slice" start end))

; splits a string
(defn split (s splitter)
  (js-call s "split" splitter))

; joins a collection
(defn join (coll s)
  (js-call coll "join" s))

; does a function for each elem in the collection
(defn foreach (coll f)
  (js-call coll "forEach" f))

; maps the collection via some function
(defn map (coll f)
  (js-call coll "map" f))

; filters the collection via some function
(defn filter (coll f)
  (js-call coll "filter" f))

; reduces the collection via some aggregate function
(defn reduce (coll f)
  (js-call coll "reduce" f))

; reduce from the right
(defn reduce-r (coll f)
  (js-call coll "reduceRight" f))

; folds the collection via some aggregate function and starting value
(defn fold (coll f init)
  (js-call coll "reduce" f init))

; folds from the right
(defn fold-r (coll f init)
  (js-call coll "reduceRight" f init))

; string upper
(defn upper (s)
  (js-call s "toUpperCase"))

; string lower
(defn lower (s)
  (js-call s "toLowerCase"))

; list application of function arguments
(defn app (f args)
  (js-call f "apply" null args))

; logs a message to the console, returns the message
(defn log (msg)
  (do
    (js-call console "log" msg)
    msg))

; logs an error to the console, returns the error
(defn err (msg)
  (do
    (js-call console "error" msg)
    msg))

; Quidditch in-language library functions
; this is where lisp is somewhat nice :)

; builds a list
(defn list (& args)
  args)

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

; gets the item at the index of the list
(defn at (l i)
  (js "l[i]"))

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
  (fn (& args)
    (a (app b args))))

; replace except using split
(defn resplit (s mat val)
  (join (split s mat) val))

; builds a string
(defn str (& args)
  (join args ""))

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

; TRAMPOLINES

; construct a trampoline call simply
(defn callCons (f args)
  (list "trampoline-call" f args))

; convenience constructor to go to the next function
(defn callT (f & args)
  (callCons f args))

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

; either

; builds a left value
(defn left (val)
  (list "left" val))

; builds a right value
(defn right (val)
  (list "right" val))

; checks if value is left
(defn is-left (either)
  (== (head either) "left"))

; checks if value is right
(defn is-right (either)
  (== (head either) "right"))

; get the value in an either
(defn either-get (either)
  (head (tail either)))
