(ns quidditch/core/native)

; creates a mutable js object
(defn new-object ()
  {})

; gets the key of an object
(defn obj-get (obj key)
  obj[key])

; sets the key of an object to the given value
(defn obj-set! (obj key value)
  (do
    obj[key]=value
    obj))

; checks if the obj has a key
(defn has-key (obj key)
  (&& obj[key] (obj.hasOwnProperty key)))

; builds a list from a head and tail
(defn cons (a1 a2)
  ([a1].concat a2))

; concats the given lists, second after first
(defn conc (a1 a2)
  (a1.concat a2))

; gets the length of a collection
(defn len (coll)
  coll.length)

; gets the index of an item in a collection
(defn index-of (coll i)
  (coll.indexOf i))

; replaces something in a string
(defn replace (s mat val)
  (s.replace mat val))

; gets the first element of a collection
(defn head (l)
  l[0])

; gets the object string for an object
(defn obj-str (obj)
  (Object.prototype.toString.call obj))

; checks if the string matches the given regex
(defn matches (stri regex)
  (stri.match regex))

; slices a collection from start to an end
(defn slice (l start end)
  (l.slice start end))

; splits a string
(defn split (s splitter)
  (s.split splitter))

; joins a collection
(defn join (coll s)
  (coll.join s))

; does a function for each elem in the collection
(defn foreach (coll f)
  (coll.forEach f))

; maps the collection via some function
(defn map (coll f)
  (coll.map f))

; reduces the collection via some aggregate function
(defn reduce (coll f)
  (coll.reduce f))

; reduce from the right
(defn reduce-r (coll f)
  (coll.reduceRight f))

; folds the collection via some aggregate function and starting value
(defn fold (coll f init)
  (coll.reduce f init))

; folds from the right
(defn fold-r (coll f init)
  (coll.reduceRight f init))

; string upper
(defn upper (s)
  (s.toUpperCase))

; string lower
(defn lower (s)
  (s.toLowerCase))

; coerces into an array, welcome to JS
; mostly used for arguments
(defn to-array (coll)
  (Array.prototype.slice.call coll))

; list application of function arguments
(defn app (f args)
  (f.apply null args))

; logs a message to the console
(defn log (msg)
  (console.log msg))

; logs an error to the console
(defn err (msg)
  (console.error msg))

; gets the process args
(def argv process.argv)