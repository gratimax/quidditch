; quidditch "native functions"
; because the runtime is pretty horrible, these functions need to exist

; creates a mutable js object
(def object (fn ()
  {}))

; gets the key of an object
(def get (fn (obj key)
  obj[key]))

; sets the key of an object to the given value
(def set (fn (obj key value)
  (do
    obj[key]=value
    obj)))

; checks if the obj has a key
(def hasKey (fn (obj key)
  (&& obj[key] (obj.hasOwnProperty key))))

; builds a list from a head and tail
(def cons (fn (a1 a2)
  ([a1].concat a2)))

; concats the given lists, second after first
(def conc (fn (a1 a2)
  (a1.concat a2)))

; gets the length of a collection
(def len (fn (coll)
  coll.length))

; gets the first element of a collection
(def head (fn (l)
  l[0]))

; gets the object string for an object
(def toObjStr (fn (obj)
  (Object.prototype.toString.call obj)))

; checks if the string matches the given regex
(def matches (fn (stri regex)
  (stri.match regex)))

; slices a collection from start to an end
(def slice (fn (l start end)
  (l.slice start end)))

; splits a string
(def split (fn (s splitter)
  (s.split splitter)))

; joins a collection
(def join (fn (coll s)
  (coll.join s)))

; does a function for each elem in the collection
(def foreach (fn (coll f)
  (coll.forEach f)))

; maps the collection via some function
(def map (fn (coll f)
  (coll.map f)))

; reduces the collection via some aggregate function
(def reduce (fn (coll f)
  (coll.reduce f)))

; reduce from the right
(def reduceR (fn (coll f)
  (coll.reduceRight f)))

; folds the collection via some aggregate function and starting value
(def fold (fn (coll f init)
  (coll.reduce f init)))

; folds from the right
(def foldR (fn (coll f init)
  (coll.reduceRight f init)))

; coerces into an array, welcome to JS
; mostly used for arguments
(def toArray (fn (coll)
  (Array.prototype.slice.call coll)))

; list application of function arguments
(def app (fn (f args)
  (f.apply null args)))

; logs a message to the console
(def log (fn (msg)
  (console.log msg)))

; logs an error to the console
(def err (fn (msg)
  (console.error msg)))

; gets the process args
(def argv process.argv)
; Quidditch in-language library functions
; this is where lisp is somewhat nice :)

; builds a list
(def list (fn ()
  (toArray arguments)))

; checks if the list is empty
(def empty (fn (l)
  (== (len l) 0)))

; gets the first elements of an array
(def init (fn (l)
  (slice l 0 (- (len l) 1))))

; gets the last element of an array
(def last (fn (l)
  (at l (- (len l) 1))))

; puts the element at the end of the list
(def push (fn (l elem)
  (conc l (list elem))))

; checks for existence, whether an item is inside a collection
(def has (fn (l item)
  (if (empty l)
    false
    (if (== (head l) item)
      true
      (has (tail l) item)))))

; gets the item and the
(def at (fn (l i)
  (if (empty l)
    (err "cannot at from empty list")
    (if (== i 0)
      (head l)
      (at (tail l) (- i 1))))))

; slices a collection from a start
(def from (fn (l start)
  (slice l start (len l))))

; gets the tail of a collection
(def tail (fn (l)
  (from l 1)))

; inclusive range from 0
(def irange (fn (end)
  (if (== end 0)
    (list 0)
    (push (irange (- end 1)) end))))

; exclusive range from 0
(def range (fn (end)
  (init (irange end))))

; zips two same-length collections into one of lists
(def zip (fn (coll1 coll2) 
  (if (== (len coll1) (len coll2))
    (map (range (len coll1)) (fn (i) (list (at coll1 i) (at coll2 i))))
    (err "collections must be same size"))))

; takes a number of elements
(def take (fn (i coll)
  (if (== i 0)
    nil
    (push (take (- i 1) coll) (at coll i)))))

; groups every two elements together
(def group2 (fn (coll)
  (if (empty coll)
    nil
    (cons (list (head coll) (at coll 1)) (group2 (from coll 2))))))

; reverses the collection
(def reverse (fn (coll)
  (fold coll (fn (acc cur)
    (cons cur acc)) nil)))

; composes two functions together
(def comp (fn (a b)
  (fn () 
    (a (app b (toArray arguments))))))

; builds a string
(def str (fn ()
  (join (toArray arguments) "")))

; checks if an object is an array
(def isArray (fn (obj)
  (== (toObjStr obj) "[object Array]")))

; checks if an object is a string
(def isString (fn (obj)
  (== (toObjStr obj) "[object String]")))

(def parens (list "(" ")"))
(def whitespace (list " " "\n"))

; skips as many whitespace chars as possible from the beginning of the string
(def skipSpace (fn (stri)
  (if (empty stri)
    stri
    (if (has whitespace (head stri))
      ; if we have more whitespace, keep going
      (skipSpace (tail stri))
      ; no more whitespace, return
      stri))))

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
(def tokenToVal (fn (token)
  (if (matches token INT_REGEX)
    token
    (if (matches token STR_REGEX)
      token
      (if (has bools token)
        token
        (quote token))))))

; quotes a token
(def quote (fn (token)
  (str "'" token)))

; "unquotes" a token by taking out the char in front
(def unquote (fn (token)
  (slice token 1)))

; checks if a token is quoted
(def isQuoted (fn (token)
  (== (head token) "'")))

; the internal recursive token accumulator.
; takes the result (a list of tokens) and the string that is being processed
(def tokenRec (fn (res stri)
  (if (empty stri)
    ; if the string is empty, we're finished
    res
    (do
      (def noSpaces (skipSpace stri))
      (def hd (head noSpaces))
      (def tl (tail noSpaces))
      (if (has parens hd)
        ; if the token is one of the parentheses, add it automatically
        (tokenRec (push res hd) tl)
        (if (== hd "\"")
          ; if the token is the double quote, then we need to do a string
          (nextOfString "" false res tl)
          (if (== hd ";")
            ; if the character is a semicolon, start comment
            (nextOfComment res tl)
            ; otherwise, let's try to make a value
            (nextOfVal hd res tl))))))))

; recursive function that consumes a comment
; takes the result list of tokens and the string remaining (comment text is ignored)
(def nextOfComment (fn (res stri)
  (if (empty stri)
    ; if the string is empty, return
    res
    (do
      (def hd (head stri))
      (def tl (tail stri))
      (if (== hd '\n')
        ; if the char is a new line (end of comment), return
        (tokenRec res tl)
        ; otherwise, we keep going
        (nextOfComment res tl))))))

; recursive function that builds up a value, meaning integer or name
; takes the result list of tokens, the accumulated string of the value,
; and the string remaining
(def nextOfVal (fn (acc res stri)
  (if (empty stri)
    ; if the string is empty, dump our accumulator and return
    (push res acc)
    (do
      (def hd (head stri))
      (def tl (tail stri))
      (if (has whitespace hd)
        ; if the char is whitespace, then we're done with this name.
        ; dump the accumulator and return
        (tokenRec (push res acc) tl)
        (if (has parens hd)
          ; if the char is parentheses, then we're done with the name
          ; dump the accumulator and return
          (tokenRec (push res acc) stri)
          ; otherwise, we keep going. append the character and continue
          (nextOfVal (str acc hd) res tl)))))))

; recursive function that builds up a string
; takes an accumulated string
(def nextOfString (fn (acc escape res stri)
  (if (empty stri)
    (err "next of string expected more characters")
    (do
      (def hd (head stri))
      (def tl (tail stri))
      (if (&& (== hd "\\") (! escape))
        ; if the character is a slash and we haven't already set escape, then do so
        (nextOfString (str acc hd) true res tl)
        (if (&& (== hd "\"") (! escape))
          ; if the character is a double quote without escape, we're done
          ; dump the accumulator, add the quotations, and return
          (tokenRec (push res (str "\"" acc "\"")) tl)
          ; otherwise, keep going and adding to the string
          (nextOfString (str acc hd) false res tl)))))))

; the final tokenize function, that starts with no tokens
(def tokenize (fn (stri)
  (tokenRec nil stri)))

; builds a parse tree, takes a token list
(def parse (fn (tokens)
  (parseRec nil tokens)))

; the parse tree recursive builder, accumulating result and remaining tokens
(def parseRec (fn (tree tokens)
  (if (empty tokens)
    ; if there's no more tokens, we're finished
    tree
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        (err "error: did not expect closing parentheses")
        (if (== hd "(")
          ; if we reach an opening parentheses, start parsing statements
          ; we push an empty list to the tree for the new s-expression
          (parseStmt 1 (push tree nil) tl)
          ; otherwise push the token to the tree and keep going
          (parseRec (push tree (tokenToVal hd)) tl)))))))

; utility function to append stuff to nested lists from the very bottom
(def pushFromBottom (fn (coll item place)
  (if (== place 0)
    ; if we're at the bottom, push the item in
    (push coll item)
    ; otherwise, we need to keep going.
    ; since we're modifying the last item of the list, we pop it off, modify it, and push it back on
    (push 
      (init coll) 
      ; do pushFromBottom, going down a place
      (pushFromBottom (last coll) item (- place 1))))))

; parses a statement
; takes the current place (much like a stack), the accumulated parse tree, and tokens
(def parseStmt (fn (place tree tokens)
  (if (empty tokens)
    (err "error: expected more, unbalanced parentheses")
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        ; if we encounter a closing brace, 
        (if (== place 1)
          ; if we're at the bottom-most statement, then we go back to the root
          (parseRec tree tl)
          ; otherwise we go down a statement
          (parseStmt (- place 1) tree tl))
        ; if we encounter an opening brace,
        (if (== hd "(")
          ; nest it further
          ; go up one place, and insert a new empty list for the new s-expression
          (parseStmt (+ place 1) (pushFromBottom tree nil place) tl)
          ; otherwise, keep going
          ; put the latest token into the nested lists
          (parseStmt place (pushFromBottom tree (tokenToVal hd) place) tl)))))))

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
    (do 
      (def mapper (comp (fn (s) (str s ";\n")) compileExpr))
      (def mapped (map (init tree) mapper))
      (def joined (join mapped ""))
      (str "(function(){\n"
        joined
        "return " (compileExpr (last tree))
        ";})()")))))

; the fn macro
(set macros "fn" (fn (tree)
  (if (== (len tree) 2)
    ; of the form (fn <expr> <expr>)
    (do
      (def argsList (map (head tree) unquote))
      (def joinedArgs (join argsList ", "))
      (def expr (compileExpr (at tree 1)))
      (str "function(" joinedArgs "){ return " expr "; }"))
    (err "fn takes two args"))))

; the defn macro
(set macros "defn" (fn (tree)
  (if (== (len tree) 3)
    ; of the form (defn <name> <args> <expr>)
    (do
      (def name (head tree))
      (def args (at tree 1))
      (def body (at tree 2))
      (def anon (list (quote "fn") args body))
      (compileExpr (list (quote "def") name anon)))
    (err "defn takes three args"))))

; the let macro
(set macros "let" (fn (tree)
  (if (== (len tree) 0)
    (err "let takes at least one arg")
    (do
      (def expr (last tree))
      (def body (map (init tree) (fn (tree)
        (cons (quote "def") tree))))
      (def form (push (cons (quote "do") body) expr))
      (compileExpr form body)))))

; the cond macro
(set macros "cond" (fn (tree)
  ; TODO when I get the >= operator in here, use that
  ; also mod
  (if (== (len tree) 0)
    (err "cond takes at least two args")
    (do
      (def lt (last tree))
      (def rest (slice tree 0 (- (len tree) 2)))
      (def grouped (group2 rest))
      (def form (foldR grouped (fn (acc con)
        (list (quote "if") (head con) (at con 1) acc)) lt))
      (compileExpr form)))))

; attempts to run a macro
(def tryMacros (fn (key tree)
  (if (hasKey macros key)
    (do 
      (def mac (get macros key))
      (mac (tail tree)))
    tree)))

; takes a parse tree and compiles it into JS
(def compile (fn (tree)
  (if (empty tree)
    ""
    (str 
      ; compiles the next expression
      (compileExpr (head tree)) 
      ";\n"
      (compile (tail tree))))))

; compiles a parse tree
(def compileExpr (fn (expr)
  (if (isArray expr)
    ; we have either an s-expression or a macro to expand
    (do 
      (def hd (head expr))
      (def tl (tail expr))
      (if (isString hd)
        ; our head is a string, meaning it's either a macro
        ; or easily compilable s-expression
        (do
          (def unquoted (unquote hd))
          (if (hasKey macros unquoted)
            ; we have found a macro to expand
            (do 
              (def mac (get macros unquoted))
              (mac tl))
            ; no macro, just plain s-expression
            (do
              (def mapped (map tl compileExpr))
              (def joined (join mapped ", "))
              (str unquoted "(" joined ")"))))
        ; otherwise, we need to go the long way and compile the head
        ; for the s-expression
        (do 
          (def callee (compileExpr hd))
          (def mapped (map tl compileExpr))
              (def joined (join mapped ", "))
          (str "(" callee ")(" mapped ")"))))
      ; we have a string
      (if (isQuoted expr)
        ; perhaps some more sophisticated deref in the future
        ; for now, a rather simple unquote
        (if (== expr (quote "nil"))
          ; nil is a special value
          "[]"
          (unquote expr))
        ; no other special forms, so we just return the expr
        expr))))

; the main function. Takes a list of string args
(def main (fn (args)
  (do 
    (def fs (require "fs"))
    (def util (require "util"))

    (def file (fs.readFileSync (at args 2) "utf-8"))

    (def tokens (tokenize file))
    (def parsed (parse tokens))
    (def compiled (compile parsed))

    ; (log (util.inspect parsed {depth:null}))

    (log compiled))))

; run the main function with the passed-in command line args
(main argv)