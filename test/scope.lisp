(def my-var 5)

(defn my-fn (x) x)

(def my-other-var
  (do
    (def should-not-be-in-root 5)))

(def x (fn (y)
  (do
    (def should-not-be-in-root 6)
    )))