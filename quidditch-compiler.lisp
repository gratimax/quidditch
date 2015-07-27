; quidditch "native functions"
; because the runtime is pretty horrible, these functions need to exist

; calls a js function
(defn js-call (obj f)
  (js "obj[f].apply(obj, Array.prototype.slice.call(arguments, 2))"))

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

; gets the process args
(def argv (js-get process "argv"))

; Quidditch in-language library functions
; this is where lisp is somewhat nice :)

; create a mutable variable
(defn new-var (initial)
  (do
    (def *var* (new-object))
    (obj-set! *var* "value" initial)
    *var*))

; sets a variable to a value
(defn var-set! (*var* value)
  (obj-set! *var* "value" value))

; gets the value of a variable
(defn var-get (*var*)
  (obj-get *var* "value"))

; applies and sets a function to the variable
(defn var-swap! (*var* f)
  (let
    (cur-val (var-get *var*))
    (new-val (f cur-val))
    (var-set! *var* new-val)))

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

(def parens (list "(" ")"))
(def whitespace (list " " "\n"))

; skips as many whitespace chars as possible from the beginning of the string
(defn skip-space (stri)
  (cond
    (empty stri)
      stri
    (has whitespace (head stri))
      ; if we have more whitespace, keep going
      (skip-space (tail stri))
    else
      ; no more whitespace, return
      stri))

; the internal recursive token accumulator.
; takes the result (a list of tokens) and the string that is being processed
(defn token-rec (res stri)
  (let
    (no-spaces (skip-space stri))
    (if (empty no-spaces)
      ; if the string is empty, we're finished
      (valT res)
      (let

        (hd (head no-spaces))
        (tl (tail no-spaces))
        (cond
          (has parens hd)
            ; if the token is one of the parentheses, add it automatically
            (callT token-rec (push res hd) tl)

          (== hd "\"")
            ; if the token is the double quote, then we need to do a string
            (callT next-of-string "" false res tl)

          (== hd ";")
            ; if the character is a semicolon, start comment
            (callT next-of-comment res tl)

          else
            ; otherwise, let's try to make a value
            (callT next-of-val hd res tl))))))


; recursive function that consumes a comment
; takes the result list of tokens and the string remaining (comment text is ignored)
(defn next-of-comment (res stri)
  (if (empty stri)
    ; if the string is empty, return
    (valT res)
    (let
      (hd (head stri))
      (tl (tail stri))
      (if (== hd "\n")
        ; if the char is a new line (end of comment), return
        (callT token-rec res tl)
        ; otherwise, we keep going
        (callT next-of-comment res tl)))))

; recursive function that builds up a value, meaning integer or name
; takes the result list of tokens, the accumulated string of the value,
; and the string remaining
(defn next-of-val (acc res stri)
  (if (empty stri)
    ; if the string is empty, dump our accumulator and return
    (valT (push res acc))
    (let
      (hd (head stri))
      (tl (tail stri))
      (cond
        (has whitespace hd)
          ; if the char is whitespace, then we're done with this name.
          ; dump the accumulator and return
          (callT token-rec (push res acc) tl)

        (has parens hd)
          ; if the char is parentheses, then we're done with the name
          ; dump the accumulator and return
          (callT token-rec (push res acc) stri)

        else
          ; otherwise, we keep going. append the character and continue
          (callT next-of-val (str acc hd) res tl)))))

; recursive function that builds up a string
; takes an accumulated string
(defn next-of-string (acc escape res stri)
  (if (empty stri)
    (err "next of string expected more characters")
    (let
      (hd (head stri))
      (tl (tail stri))
      (cond
        (&& (== hd "\\") (! escape))
          ; if the character is a slash and we haven't already set escape, then do so
          (callT next-of-string (str acc hd) true res tl)

        (&& (== hd "\"") (! escape))
          ; if the character is a double quote without escape, we're done
          ; dump the accumulator, add the quotations, and return
          (callT token-rec (push res (str "\"" acc "\"")) tl)

        else
          ; otherwise, keep going and adding to the string
          (callT next-of-string (str acc hd) false res tl)))))

; the final tokenize function, that starts with no tokens
(defn tokenize (stri)
  (runT token-rec (list nil stri)))

(def INT_REGEX (str
  "^" ; match start
  "(-)?" ; optional negative sign
  "[0-9]+" ; a digit
  "(" ; optional positive exponential
  "(?:E|e)" ; use e or E
  "[0-9]+)?" ; at least 1 digit
  "$" ; match end
  ))

(def STR_REGEX (str
  "^" ; match start
  "\"" ; match opening quote
  ".*" ; match any character in between
  "\"" ; match closing quote
  "$" ; match end
  ))

; quotes a token
(defn quote (token)
  (str "'" token))

; "unquotes" a token by taking out the char in front
(defn unquote (token)
  (slice token 1))

; checks if a token is quoted
(defn is-quoted (token)
  (== (head token) "'"))

(def bools (list "true" "false"))

; convert a plaintext token into a parse tree token
(defn token-to-val (token)
  (cond
    (matches token INT_REGEX) token
    (matches token STR_REGEX) token
    (has bools token) token
    else (quote token)))

; the parse tree recursive builder, accumulating result and remaining tokens
(defn parse-rec (tree tokens)
  (if (empty tokens)
    ; if there's no more tokens, we're finished
    tree
    (let
      (hd (head tokens))
      (tl (tail tokens))
      (cond
        (== hd ")")
          (err "error: did not expect closing parentheses")

        (== hd "(")
          ; if we reach an opening parentheses, start parsing statements
          ; we push an empty list to the tree for the new s-expression
          (parse-stmt 1 (push tree nil) tl)

        else
          ; otherwise push the token to the tree and keep going
          (parse-rec (push tree (token-to-val hd)) tl)))))

; utility function to append stuff to nested lists from the very bottom
(defn push-from-bottom (coll item place)
  (if (== place 0)
    ; if we're at the bottom, push the item in
    (push coll item)
    ; otherwise, we need to keep going.
    ; since we're modifying the last item of the list, we pop it off, modify it, and push it back on
    (push
      (init coll)
      ; do push-from-bottom, going down a place
      (push-from-bottom (last coll) item (- place 1)))))

; parses a statement
; takes the current place (much like a stack), the accumulated parse tree, and tokens
(defn parse-stmt (place tree tokens)
  (if (empty tokens)
    (err "error: expected more, unbalanced parentheses")
    (let
      (hd (head tokens))
      (tl (tail tokens))
      (cond
        ; if we encounter a closing brace,
        (== hd ")")
          (if (== place 1)
            ; if we're at the bottom-most statement, then we go back to the root
            (parse-rec tree tl)
            ; otherwise we go down a statement
            (parse-stmt (- place 1) tree tl))

        ; if we encounter an opening brace,
        (== hd "(")
          ; nest it further
          ; go up one place, and insert a new empty list for the new s-expression
          (parse-stmt (+ place 1) (push-from-bottom tree nil place) tl)

        else
          ; otherwise, keep going
          ; put the latest token into the nested lists
          (parse-stmt place (push-from-bottom tree (token-to-val hd) place) tl)))))

; builds a parse tree, takes a token list
(defn parse (tokens)
  (parse-rec nil tokens))

; replaces the exclamation point out
(defn repl-chars (name)
  (resplit (resplit (resplit name "?" "Ques") "!" "Excl") "*" "Astsk"))

; sanitizes names, removes dashes and exclamation points
(defn sanitize (name)
  (let
    (replaced (repl-chars name))
    ; find the next dash
    (index-dash (index-of replaced  "-"))
    (if (== -1 index-dash)
      ; if it's not there, then we're done
      replaced
      ; otherwise keep going
      (str
        (slice replaced 0 index-dash)
        (upper (at replaced (+ index-dash 1)))
        (sanitize (slice replaced (+ index-dash 2)))))))

; whether the expression currently evaluated is at the root
(def *at-root* (new-var false))

; the current exports
(def *exports* (new-var nil))

; whether the generated code should have newline?s
(def pretty? false)

; the optional newline? character
(def newline? (if pretty? "\n" ""))

; the list of compiler macros
(def macros (new-object))

; unary negation operator
(obj-set! macros "!" (fn (tree)
  (if (== (len tree) 1)
    ; of the form (! <expr>)
    (str "!(" (compile-expr (head tree)) ")")
    (err "! takes one arg"))))

; list of standard infix ops
(def infix (list "+" "-" "/" "*" "&&" "||" "<" ">" "<=" ">="))

; make a macro for all infix operators
(foreach infix (fn (infix)
  (obj-set! macros infix (fn (tree)
    (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compile-expr (head tree)) infix (compile-expr (at tree 1)) ")")
      (err (str infix " takes two args")))))))

; == gets special treatment since it desugars to === always
(obj-set! macros "==" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compile-expr (head tree)) "===" (compile-expr (at tree 1)) ")")
      (err "== takes two args"))))

; same with !=
(obj-set! macros "!=" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compile-expr (head tree)) "!==" (compile-expr (at tree 1)) ")")
      (err "!= takes two args"))))

; and mod
(obj-set! macros "mod" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compile-expr (head tree)) "%" (compile-expr (at tree 1)) ")")
      (err "mod takes two args"))))

; the def macro
(obj-set! macros "def" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (def <name> <expr>)
    (do
      (def name (sanitize (unquote (head tree))))
      (if (var-get *at-root*)
        (var-swap! *exports* (fn (exports)
          (push exports name)))
        nil)
      (str
        "var "
        name
        " = "
        (compile-expr (at tree 1))))
    (err "def takes two args"))))

; the if macro
(obj-set! macros "if" (fn (tree)
  (if (== (len tree) 3)
    ; of the form (if <expr> <expr> <expr>)
    (str
      "(" (compile-expr (head tree))
      " ? " (compile-expr (at tree 1))
      " : " (compile-expr (at tree 2))
      ")")
    (err "if takes three args"))))

; the do macro
(obj-set! macros "do" (fn (tree)
  (if (== (len tree) 0)
    ; of the form (do <expr>+)
    (err "do takes at least one arg")
    (let
      (_ (var-set! *at-root* false))
      (mapper (comp (fn (s) (str s ";" newline?)) compile-expr))
      (mapped (map (init tree) mapper))
      (joined (join mapped ""))
      (str "(function(){" newline?
        joined
        "return " (compile-expr (last tree))
        ";})()")))))

; the fn macro
(obj-set! macros "fn" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (fn <expr> <expr>)
    (let
      (unquote-then-sanitize (comp sanitize unquote))
      (hd (head tree))
      (len-hd (len hd))
      (has-splat (&& (>= len-hd 2) (== (at hd (- len-hd 2)) (quote "&"))))
      (splat-name (if has-splat
        (unquote-then-sanitize (at hd (- len-hd 1)))
        ""))
      (plain-args (if has-splat
        (init (init hd))
        hd))
      (argsList (map plain-args unquote-then-sanitize))
      (joinedArgs (join argsList ", "))
      (splat-handling (if has-splat
        (str "var " splat-name " = Array.prototype.slice.call(arguments," (len argsList) ");" newline?)
        ""))
      (expr (compile-expr (at tree 1)))
      (str "function(" joinedArgs "){ " splat-handling "return " expr "; }"))
    (err "fn takes two args"))))

; the defn macro
(obj-set! macros "defn" (fn (tree)
  (if (== (len tree) 3)
    ; of the form (defn <name> <args> <expr>)
    (let
      (name (head tree))
      (args (at tree 1))
      (body (at tree 2))
      (anon (list (quote "fn") args body))
      (compile-expr (list (quote "def") name anon)))
    (err "defn takes three args"))))

; the let macro
(obj-set! macros "let" (fn (tree)
  (if (== (len tree) 0)
    (err "let takes at least one arg")
    (let
      (expr (last tree))
      (body (map (init tree) (fn (tree)
        (cons (quote "def") tree))))
      (form (push (cons (quote "do") body) expr))
      (compile-expr form body)))))

; the cond macro
(obj-set! macros "cond" (fn (tree)
  (cond
    (!= (mod (len tree) 2) 0)
      (err "cond must take an even number of params")

    (<= (len tree) 2)
      (err "cond takes at least two args")

    else
      (let
        (lt (last tree))
        (rest (slice tree 0 (- (len tree) 2)))
        (grouped (group2 rest))
        (form (fold-r grouped (fn (acc con)
          (list (quote "if") (head con) (at con 1) acc)) lt))
        (compile-expr form)))))

; while macro
(obj-set! macros "while" (fn (tree)
  (if (>= (len tree) 2)
    ; of the form (while <expr> <ex1> <ex2> <ex3>)
    (let
      (mapper (comp (fn (s) (str s ";" newline?)) compile-expr))
      (mapped (map (tail tree) mapper))
      (joined (join mapped ""))
      (str
        "while(" (compile-expr (head tree)) "){" newline?
        joined
        "}"))
    (err "while takes at least two args"))))

; set! macro
(obj-set! macros "set!" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (set! <name> <val>)
    (let
      (name (sanitize (unquote (head tree))))
      (body (compile-expr (at tree 1)))
      (str name " = " body))
    (err "set takes only two args"))))

; js eval macro, passes string straight through
(obj-set! macros "js" (fn (tree)
  (if (== (len tree) 1)
    ; of the form (js "<js>")
    (init (tail (head tree)))
    (err "js takes only one arg"))))

; takes a parse tree and compiles it into JS
(defn compile (tree)
  (if (empty tree)
    (let
      ; assemble exports
      (exp (var-get *exports*))
      (exp-list (join exp " "))
      ;(setters (map exp (fn (e) (str "export('" e "', " e ");" newline?))))
      (setters nil)
      (setters-joined (join setters ""))
      (str
        setters-joined
        "// QUIDDITCH-EXPORTS " exp-list newline?))
    (do
      (var-set! *at-root* true)
      (str
        ; compiles the next expression
        (compile-expr (head tree))
        ";" newline?
        (compile (tail tree))))))

; compiles a parse tree
(defn compile-expr (expr)
  (cond
    ; we have either an s-expression or a macro to expand
    (is-array expr)
      (let
        (hd (head expr))
        (tl (tail expr))
        (if (is-string hd)
          ; our head is a string, meaning it's either a macro
          ; or easily compilable s-expression
          (let
            (unquoted (unquote hd))
            (if (has-key macros unquoted)
              ; we have found a macro to expand
              ((obj-get macros unquoted) tl)
              ; no macro, just plain s-expression
              (let
                (mapped (map tl compile-expr))
                (joined (join mapped ", "))
                (str (sanitize unquoted) "(" joined ")"))))
          ; otherwise, we need to go the long way and compile the head
          ; for the s-expression
          (let
            (callee (compile-expr hd))
            (mapped (map tl compile-expr))
            (joined (join mapped ", "))
            (str "(" callee ")(" joined ")"))))

    ; we have a string
    (is-quoted expr)
      (if (== expr (quote "nil"))
        ; nil is a special value
        "[]"
        ; perhaps some more sophisticated deref in the future
        ; for now, a rather simple unquote
        (unquote (sanitize expr)))

    ; no other special forms, so we just return the expr
    else expr))

; runs the compiler on some code
(defn run-compiler (source)
  (let
    (tokens (tokenize source))
    (parsed (parse tokens))
    (compiled (compile parsed))
    compiled))

; the main function. Takes a list of string args
(defn main (args)
  (let
    (util (require "util"))

    (len-args (len args))

    (cond

      ; we are not given anything, start the repl
      (== len-args 2)
        (do
          (def stdin (js-get process "stdin"))
          (def stdout (js-get process "stdout"))
          (def vm (require "vm"))
          (def sandbox (js-call vm "createContext"))
          (log "Quidditch repl (ctrl-c to exit)")
          (js-call stdout "write" "> ")
          (js-call stdin "setEncoding" "utf8")
          (js-call stdin "on" "readable" (fn ()
            (let
              (chunk (js-call stdin "read"))
              (if (!= chunk null)
                (let
                  (compiled (run-compiler chunk))
                  ; should use custom evaluator later on
                  (evaled (js-call vm "runInContext" compiled sandbox))
                  (inspected (js-call util "inspect" evaled (js "{depth:null}")))
                  (js-call stdout "write" (str inspected "\n> ")))
                nil)))))

      ; we are given a file, compile it
      (== len-args 3)
        (let
          (fs (require "fs"))
          (file (js-call fs "readFileSync" (at args 2) "utf-8"))
          (compiled (run-compiler file))
          (log compiled)))))

; run the main function with the passed-in command line args
(main argv)
