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

; slices a collection from start to an end
(def slice (fn (l start end)
  (l.slice start end)))

; gets the first element of a string
(def strHead (fn (s)
  (s.charAt 0)))

; gets the tail of a string
(def strTail (fn (s)
  (s.substring 1)))

; splits a string
(def split (fn (s splitter)
  (s.split splitter)))

; joins a collection
(def join (fn (coll s)
  (coll.join s)))

; coerces into an array, welcome to JS
; mostly used for arguments
(def toArray (fn (coll)
  (Array.prototype.slice.call coll)))

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

; backwards application of function arguments
(def app (fn (arg l)
  (l arg)))

; builds a string
(def str (fn ()
  (join (toArray arguments) "")))

(def parens (list "(" ")"))
(def whitespace (list " " "\n"))
(def infix (list "==" "+" "-" "/" "*" "&&" "||"))

; skips as many whitespace chars as possible from the beginning of the string
(def skipSpace (fn (stri)
  (if (empty stri)
    stri
    (if (has whitespace (strHead stri))
      ; if we have more whitespace, keep going
      (skipSpace (strTail stri))
      ; no more whitespace, return
      stri))))

(def nextOfString)
(def nextOfVal)

; the internal recursive token accumulator.
; takes the result (a list of tokens) and the string that is being processed
(def tokenRec (fn (res stri)
  (if (empty stri)
    ; if the string is empty, we're finished
    res
    (do
      (def noSpaces (skipSpace stri))
      (def hd (strHead noSpaces))
      (def tl (strTail noSpaces))
      (if (has parens hd)
        ; if the token is one of the parentheses, add it automatically
        (tokenRec (conc res (list hd)) tl)
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
      (def hd (strHead stri))
      (def tl (strTail stri))
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
    (conc res (list acc))
    (do
      (def hd (strHead stri))
      (def tl (strTail stri))
      (if (has whitespace hd)
        ; if the char is whitespace, then we're done with this name.
        ; dump the accumulator and return
        (tokenRec (conc res (list acc)) tl)
        (if (has parens hd)
          ; if the char is parentheses, then we're done with the name
          ; dump the accumulator and return
          (tokenRec (conc res (list acc)) stri)
          ; otherwise, we keep going. append the character and continue
          (nextOfVal (str acc hd) res tl)))))))

; recursive function that builds up a string
; takes an accumulated string
(def nextOfString (fn (acc escape res stri)
  (if (empty stri)
    ; cannot have an unclosed string
    (err "next of string expected more characters")
    (do
      (def hd (strHead stri))
      (def tl (strTail stri))
      (if (&& (== hd "\\") (! escape))
        ; if the character is a slash and we haven't already set escape, then do so
        (nextOfString (+ acc hd) true res tl)
        (if (&& (== hd "\"") (! escape))
          ;if the character is a double quote without escape, we're done
          ;dump the accumulator, add the quotations, and return
          (tokenRec (conc res (list (str "\"" acc "\""))) tl)
          ; otherwise, keep going and adding to the string
          (nextOfString (str acc hd) false res tl)))))))

; the final tokenize function, that starts with no tokens
(def tokenize (fn (stri)
  (tokenRec nil stri)))

; creates a compile step structure that holds tokens remaining and accumulated string
(def ncompile (fn (tokens stri)
  (list tokens stri)))

; get the tokens out of the struct
(def gtokens (fn (struct)
  (head struct)))

; get the string out of the struct
(def gstr (fn (struct)
  (at struct 1)))

; creates an arguments structure
(def nargs (fn (tokens args)
  (list tokens args)))

; get the args out of the struct
(def gargs (fn (struct)
  (at struct 1)))

(def compileArgs)
(def compileAnonArgs)
(def compileDo)

(def compileExpr (fn (tokens)
  (if (empty tokens)
    (err "error: unexpected end")
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        (err "error: unexpected closing paren expr")
        (if (== hd "(")
          (compileApp tl)
          (if (== hd "nil")
            (ncompile tl "[]")
            (ncompile tl hd))))))))

(def compileApp (fn (tokens)
  (if (empty tokens)
    (err "error: unexpected end")
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        (err "error: unexpected closing paren app")
        (if (== hd "(")
          (do
            (def step (compileExpr tokens))
            (def args (compileArgs (gtokens step)))
            (def joined (join (gargs args) ", "))
            (ncompile (gtokens args) (str "(" joined ")")))
          (if (== hd "def")
            (do 
              (def args (compileArgs tl))
              (def argslist (gargs args))
              (if (== 1 (len argslist))
                (ncompile (gtokens args) (str "var " (head argslist)))
                (if (== 2 (len argslist))
                  (ncompile (gtokens args) 
                    (str "var " (head argslist) " = " (at argslist 1)))
                  (err "error: def expects one or two args"))))
            (if (has infix hd)
              (do
                (def arg1 (compileExpr tl))
                (def arg2 (compileExpr (gtokens arg1)))
                (def infix (if (== hd "==") "===" hd))
                (ncompile (tail (gtokens arg2))
                  (str "(" (gstr arg1) infix (gstr arg2) ")")))
              (if (== hd "!")
                (do
                  (def expr (compileExpr tl))
                  (ncompile (tail (gtokens expr)) 
                    (str "!(" (gstr expr) ")")))
                (if (== hd "fn")
                  (do
                    (def args (compileAnonArgs (tail tl)))
                    (def expr (compileExpr (gtokens args)))
                    (def joined (join (gargs args) ", "))
                    (ncompile (tail (gtokens expr))
                      (str "function(" joined "){ return " (gstr expr) "; }")))
                  (if (== hd "if")
                    (do
                      (def condition (compileExpr tl))
                      (def expr1 (compileExpr (gtokens condition)))
                      (def expr2 (compileExpr (gtokens expr1)))
                      (ncompile (tail (gtokens expr2))
                        (str "(" (gstr condition) " ? " (gstr expr1) " : " (gstr expr2) ")")))
                    (if (== hd "do")
                      (do
                        (def expr (compileDo nil tl))
                        (def args (gargs expr))
                        (def stmts (init args))
                        (def ret (last args))
                        (def joined (join stmts ";\n"))
                        (ncompile (gtokens expr)
                          (str "(function(){" joined ";\nreturn " ret "; })()")))
                      (do
                        (def args (compileArgs tl))
                        (def joined (join (gargs args) ", "))
                        (ncompile (gtokens args)
                          (str hd "(" joined ")")))))))))))))))

(def compileArgs (fn (tokens)
  (if (empty tokens)
    (err "error: unexpected end")
    (do
      (def hd (head tokens))
      (if (== hd ")")
        (nargs (tail tokens) nil)
        (do
          (def expr (compileExpr tokens))
          (def rest (compileArgs (gtokens expr)))
          (nargs (gtokens rest) (cons (gstr expr) (gargs rest)))))))))

; compile arguments for an anonymous function
(def compileAnonArgs (fn (tokens)
  (if (empty tokens)
    (err "error: unexpected end")
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        (nargs tl nil)
        (do
          (def rest (compileAnonArgs tl))
          (nargs (gtokens rest) (cons hd (gargs rest)))))))))

(def compileDo (fn (stmts tokens)
  (if (empty tokens)
    (err "error: unexpected end")
    (do
      (def hd (head tokens))
      (def tl (tail tokens))
      (if (== hd ")")
        (nargs tl stmts)
        (do
          (def next (compileExpr tokens))
          (compileDo (conc stmts (list (gstr next))) (gtokens next))))))))

; the final compile function
; takes a list of tokens and generates source files
(def compile (fn (tokens)
  (if (empty tokens)
    ""
    (do
      (def step (compileExpr tokens))
      (str (gstr step) ";\n" 
        (compile (gtokens step)))))))

; the main function. Takes a list of string args
(def main (fn (args)
  (do 
    (def fs (require "fs"))
    (def tokens (tokenize (fs.readFileSync (at args 2) "utf-8")))
    (def compiled (compile tokens))
    (log compiled))))

; run the main function with the passed-in args
(main argv)