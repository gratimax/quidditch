; quidditch "native functions"
; because the runtime is pretty horrible, these functions need to exist

; creates a mutable js object
(defn object ()
  {})

; gets the key of an object
(defn get (obj key)
  obj[key])

; sets the key of an object to the given value
(defn set (obj key value)
  (do
    obj[key]=value
    obj))

; checks if the obj has a key
(defn hasKey (obj key)
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

; gets the first element of a collection
(defn head (l)
  l[0])

; gets the object string for an object
(defn toObjStr (obj)
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
(defn reduceR (coll f)
  (coll.reduceRight f))

; folds the collection via some aggregate function and starting value
(defn fold (coll f init)
  (coll.reduce f init))

; folds from the right
(defn foldR (coll f init)
  (coll.reduceRight f init))

; coerces into an array, welcome to JS
; mostly used for arguments
(defn toArray (coll)
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
; Quidditch in-language library functions
; this is where lisp is somewhat nice :)

; builds a list
(defn list ()
  (toArray arguments))

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
    (a (app b (toArray arguments)))))

; builds a string
(defn str ()
  (join (toArray arguments) ""))

; checks if an object is an array
(defn isArray (obj)
  (== (toObjStr obj) "[object Array]"))

; checks if an object is a string
(defn isString (obj)
  (== (toObjStr obj) "[object String]"))

(def parens (list "(" ")"))
(def whitespace (list " " "\n"))

; skips as many whitespace chars as possible from the beginning of the string
(defn skipSpace (stri)
  (cond
    (empty stri)
      stri
    (has whitespace (head stri))
      ; if we have more whitespace, keep going
      (skipSpace (tail stri))
    else
      ; no more whitespace, return
      stri))

; the internal recursive token accumulator.
; takes the result (a list of tokens) and the string that is being processed
(defn tokenRec (res stri)
  (if (empty stri)
    ; if the string is empty, we're finished
    res
    (let
      (noSpaces (skipSpace stri))
      (hd (head noSpaces))
      (tl (tail noSpaces))
      (cond
        (has parens hd)
          ; if the token is one of the parentheses, add it automatically
          (tokenRec (push res hd) tl)

        (== hd "\"")
          ; if the token is the double quote, then we need to do a string
          (nextOfString "" false res tl)

        (== hd ";")
          ; if the character is a semicolon, start comment
          (nextOfComment res tl)

        else
          ; otherwise, let's try to make a value
          (nextOfVal hd res tl)))))

; recursive function that consumes a comment
; takes the result list of tokens and the string remaining (comment text is ignored)
(defn nextOfComment (res stri)
  (if (empty stri)
    ; if the string is empty, return
    res
    (let
      (hd (head stri))
      (tl (tail stri))
      (if (== hd '\n')
        ; if the char is a new line (end of comment), return
        (tokenRec res tl)
        ; otherwise, we keep going
        (nextOfComment res tl)))))

; recursive function that builds up a value, meaning integer or name
; takes the result list of tokens, the accumulated string of the value,
; and the string remaining
(defn nextOfVal (acc res stri)
  (if (empty stri)
    ; if the string is empty, dump our accumulator and return
    (push res acc)
    (let
      (hd (head stri))
      (tl (tail stri))
      (cond
        (has whitespace hd)
          ; if the char is whitespace, then we're done with this name.
          ; dump the accumulator and return
          (tokenRec (push res acc) tl)

        (has parens hd)
          ; if the char is parentheses, then we're done with the name
          ; dump the accumulator and return
          (tokenRec (push res acc) stri)

        else
          ; otherwise, we keep going. append the character and continue
          (nextOfVal (str acc hd) res tl)))))

; recursive function that builds up a string
; takes an accumulated string
(defn nextOfString (acc escape res stri)
  (if (empty stri)
    (err "next of string expected more characters")
    (let
      (hd (head stri))
      (tl (tail stri))
      (cond
        (&& (== hd "\\") (! escape))
          ; if the character is a slash and we haven't already set escape, then do so
          (nextOfString (str acc hd) true res tl)

        (&& (== hd "\"") (! escape))
          ; if the character is a double quote without escape, we're done
          ; dump the accumulator, add the quotations, and return
          (tokenRec (push res (str "\"" acc "\"")) tl)

        else
          ; otherwise, keep going and adding to the string
          (nextOfString (str acc hd) false res tl)))))

; the final tokenize function, that starts with no tokens
(defn tokenize (stri)
  (tokenRec nil stri))

; builds a parse tree, takes a token list
(defn parse (tokens)
  (parseRec nil tokens))

; the parse tree recursive builder, accumulating result and remaining tokens
(defn parseRec (tree tokens)
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
          (parseStmt 1 (push tree nil) tl)

        else
          ; otherwise push the token to the tree and keep going
          (parseRec (push tree (tokenToVal hd)) tl)))))

; utility function to append stuff to nested lists from the very bottom
(defn pushFromBottom (coll item place)
  (if (== place 0)
    ; if we're at the bottom, push the item in
    (push coll item)
    ; otherwise, we need to keep going.
    ; since we're modifying the last item of the list, we pop it off, modify it, and push it back on
    (push 
      (init coll) 
      ; do pushFromBottom, going down a place
      (pushFromBottom (last coll) item (- place 1)))))

; parses a statement
; takes the current place (much like a stack), the accumulated parse tree, and tokens
(defn parseStmt (place tree tokens)
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
            (parseRec tree tl)
            ; otherwise we go down a statement
            (parseStmt (- place 1) tree tl))

        ; if we encounter an opening brace,
        (== hd "(")
          ; nest it further
          ; go up one place, and insert a new empty list for the new s-expression
          (parseStmt (+ place 1) (pushFromBottom tree nil place) tl)

        else
          ; otherwise, keep going
          ; put the latest token into the nested lists
          (parseStmt place (pushFromBottom tree (tokenToVal hd) place) tl)))))

; the list of compiler macros
(def macros (object))

; unary negation operator
(set macros "!" (fn (tree)
  (if (== (len tree) 1)
    ; of the form (! <expr>)
    (str "!(" (compileExpr (head tree)) ")")
    (err "! takes one arg"))))

; list of standard infix ops
(def infix (list "+" "-" "/" "*" "&&" "||" "<" ">" "<=" ">="))

; make a macro for all infix operators
(foreach infix (fn (infix)
  (set macros infix (fn (tree)
    (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compileExpr (head tree)) infix (compileExpr (at tree 1)) ")")
      (err (str infix " takes two args")))))))

; == gets special treatment since it desugars to === always
(set macros "==" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compileExpr (head tree)) "===" (compileExpr (at tree 1)) ")")
      (err "== takes two args"))))

; same with !=
(set macros "!=" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compileExpr (head tree)) "!==" (compileExpr (at tree 1)) ")")
      (err "!= takes two args"))))

; and mod
(set macros "mod" (fn (tree)
  (if (== (len tree) 2)
      ; of the form (<op> <expr> <expr>)
      (str "(" (compileExpr (head tree)) "%" (compileExpr (at tree 1)) ")")
      (err "!= takes two args"))))

; the def macro
(set macros "def" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (def <name> <expr>)
    (str "var " (unquote (head tree)) " = " (compileExpr (at tree 1)))
    (err "def takes two args"))))

; the if macro
(set macros "if" (fn (tree)
  (if (== (len tree) 3)
    ; of the form (if <expr> <expr> <expr>)
    (str 
      "(" (compileExpr (head tree)) 
      " ? " (compileExpr (at tree 1)) 
      " : " (compileExpr (at tree 2))
      ")")
    (err "if takes three args"))))

; the do macro
(set macros "do" (fn (tree)
  (if (== (len tree) 0)
    ; of the form (do <expr>+)
    (err "do takes at least one arg")
    (let
      (mapper (comp (fn (s) (str s ";\n")) compileExpr))
      (mapped (map (init tree) mapper))
      (joined (join mapped ""))
      (str "(function(){\n"
        joined
        "return " (compileExpr (last tree))
        ";})()")))))

; the fn macro
(set macros "fn" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (fn <expr> <expr>)
    (let
      (argsList (map (head tree) unquote))
      (joinedArgs (join argsList ", "))
      (expr (compileExpr (at tree 1)))
      (str "function(" joinedArgs "){ return " expr "; }"))
    (err "fn takes two args"))))

; the defn macro
(set macros "defn" (fn (tree)
  (if (== (len tree) 3)
    ; of the form (defn <name> <args> <expr>)
    (let
      (name (head tree))
      (args (at tree 1))
      (body (at tree 2))
      (anon (list (quote "fn") args body))
      (compileExpr (list (quote "def") name anon)))
    (err "defn takes three args"))))

; the let macro
(set macros "let" (fn (tree)
  (if (== (len tree) 0)
    (err "let takes at least one arg")
    (let
      (expr (last tree))
      (body (map (init tree) (fn (tree)
        (cons (quote "def") tree))))
      (form (push (cons (quote "do") body) expr))
      (compileExpr form body)))))

; the cond macro
(set macros "cond" (fn (tree)
  ; TODO when I get the >= operator in here, use that
  ; also mod
  (if (== (len tree) 0)
    (err "cond takes at least two args")
    (let
      (lt (last tree))
      (rest (slice tree 0 (- (len tree) 2)))
      (grouped (group2 rest))
      (form (foldR grouped (fn (acc con)
        (list (quote "if") (head con) (at con 1) acc)) lt))
      (compileExpr form)))))

; attempts to run a macro
(defn tryMacros (key tree)
  (if (hasKey macros key)
    ((get macros key)(tail tree))
    tree))

(def INT_REGEX (str 
  "^" ; match start
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

(def bools (list "true" "false"))

; convert a plaintext token into a parse tree token
(defn tokenToVal (token)
  (cond
    (matches token INT_REGEX) token
    (matches token STR_REGEX) token
    (has bools token) token
    else (quote token)))

; quotes a token
(defn quote (token)
  (str "'" token))

; "unquotes" a token by taking out the char in front
(defn unquote (token)
  (slice token 1))

; checks if a token is quoted
(defn isQuoted (token)
  (== (head token) "'"))

; takes a parse tree and compiles it into JS
(defn compile (tree)
  (if (empty tree)
    ""
    (str 
      ; compiles the next expression
      (compileExpr (head tree)) 
      ";\n"
      (compile (tail tree)))))

; compiles a parse tree
(defn compileExpr (expr)
  (cond 
    ; we have either an s-expression or a macro to expand
    (isArray expr)
      (let
        (hd (head expr))
        (tl (tail expr))
        (if (isString hd)
          ; our head is a string, meaning it's either a macro
          ; or easily compilable s-expression
          (let
            (unquoted (unquote hd))
            (if (hasKey macros unquoted)
              ; we have found a macro to expand
              ((get macros unquoted) tl)
              ; no macro, just plain s-expression
              (let
                (mapped (map tl compileExpr))
                (joined (join mapped ", "))
                (str unquoted "(" joined ")"))))
          ; otherwise, we need to go the long way and compile the head
          ; for the s-expression
          (let
            (callee (compileExpr hd))
            (mapped (map tl compileExpr))
            (joined (join mapped ", "))
            (str "(" callee ")(" joined ")"))))

    ; we have a string
    (isQuoted expr)
      ; perhaps some more sophisticated deref in the future
      ; for now, a rather simple unquote
      (if (== expr (quote "nil"))
        ; nil is a special value
        "[]"
        (unquote expr))

    ; no other special forms, so we just return the expr
    else expr))

; the main function. Takes a list of string args
(defn main (args)
  (let 
    (fs (require "fs"))
    (util (require "util"))

    (file (fs.readFileSync (at args 2) "utf-8"))

    (tokens (tokenize file))
    (parsed (parse tokens))
    (compiled (compile parsed))

    ; (log (util.inspect parsed {depth:null}))

    (log compiled)))

; run the main function with the passed-in command line args
(main argv)