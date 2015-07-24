(def my-awesome-test (fn (a b) a))

(def y (my-awesome-fn))

(def z (my-awesome-fn! 2 -1))

(def q (+ my-awesome-test my-awesome-fn))

(def *my-var* 0)

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

(defn z ()
  (do
    (def x 0)
    (while (< x 10)
      (console.log x)
      (set! x (+ x 1)))
    x))

