(def fs (require "fs"))

(def slurp (fn (file)
  (fs.readFileSync file "utf-8")))

(def log (fn (str)
  (console.log str)))

(log (slurp "quidditch.qd"))