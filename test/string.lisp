(def s1 "hello how are you")
(def s2 "testing \n out \\ escapes \" \\\ ")
(def s3 "\\")

(def join (lam (coll s)
  (coll.join s)))

(def toArray (lam (coll)
  (Array.prototype.slice.call coll)))

(def list (lam ()
  (toArray arguments)))

(def str (lam ()
  (join (toArray arguments) "")))

(console.log (list 1 2 3 4))
(console.log s2)