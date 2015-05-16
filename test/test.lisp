(def x (fn (a b) a))

(def y (if true 3 4))

(def z (do
  3
  4
  (if true
    3
    4
  )))

(defn x (a b)
  (let 
    (a (+ 4 5))
    (c (! a))
    e))

(def q (cond
  a (if (a) b c)
  b 2
  else 3))

(def a ((fn (x) x) 3))