(def s1 "hello how are you")
(def s2 "testing \n out \\ escapes \" \\\ ")
(def s3 "\\")

(def join (fn (coll s)
  (coll.join s)))

(def toArray (fn (coll)
  (Array.prototype.slice.call coll)))

(def list (fn ()
  (toArray arguments)))

(def str (fn ()
  (join (toArray arguments) "")))

(console.log (list 1 2 3 4))
(console.log s2)
