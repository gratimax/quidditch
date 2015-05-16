var object = function(){ return {}; };
var objGet = function(obj, key){ return obj[key]; };
var objSetExcl = function(obj, key, value){ return (function(){
obj[key]=value;
return obj;})(); };
var vset = function(obj, value){ return (function(){
obj=value;
return obj;})(); };
var hasKey = function(obj, key){ return (obj[key]&&obj.hasOwnProperty(key)); };
var cons = function(a1, a2){ return [a1].concat(a2); };
var conc = function(a1, a2){ return a1.concat(a2); };
var len = function(coll){ return coll.length; };
var indexOf = function(coll, i){ return coll.indexOf(i); };
var replace = function(s, mat, val){ return s.replace(mat, val); };
var head = function(l){ return l[0]; };
var objStr = function(obj){ return Object.prototype.toString.call(obj); };
var matches = function(stri, regex){ return stri.match(regex); };
var slice = function(l, start, end){ return l.slice(start, end); };
var split = function(s, splitter){ return s.split(splitter); };
var join = function(coll, s){ return coll.join(s); };
var foreach = function(coll, f){ return coll.forEach(f); };
var map = function(coll, f){ return coll.map(f); };
var reduce = function(coll, f){ return coll.reduce(f); };
var reduceR = function(coll, f){ return coll.reduceRight(f); };
var fold = function(coll, f, init){ return coll.reduce(f, init); };
var foldR = function(coll, f, init){ return coll.reduceRight(f, init); };
var upper = function(s){ return s.toUpperCase(); };
var lower = function(s){ return s.toLowerCase(); };
var toArray = function(coll){ return Array.prototype.slice.call(coll); };
var app = function(f, args){ return f.apply(null, args); };
var log = function(msg){ return console.log(msg); };
var err = function(msg){ return console.error(msg); };
var argv = process.argv;
var list = function(){ return toArray(arguments); };
var empty = function(l){ return (len(l)===0); };
var init = function(l){ return slice(l, 0, (len(l)-1)); };
var last = function(l){ return at(l, (len(l)-1)); };
var push = function(l, elem){ return conc(l, list(elem)); };
var has = function(l, item){ return (empty(l) ? false : ((head(l)===item) ? true : has(tail(l), item))); };
var at = function(l, i){ return (empty(l) ? err("cannot at from empty list") : ((i===0) ? head(l) : at(tail(l), (i-1)))); };
var from = function(l, start){ return slice(l, start, len(l)); };
var tail = function(l){ return from(l, 1); };
var irange = function(end){ return ((end===0) ? list(0) : push(irange((end-1)), end)); };
var range = function(end){ return init(irange(end)); };
var zip = function(coll1, coll2){ return ((len(coll1)===len(coll2)) ? map(range(len(coll1)), function(i){ return list(at(coll1, i), at(coll2, i)); }) : err("collections must be same size")); };
var take = function(i, coll){ return ((i===0) ? [] : push(take((i-1), coll), at(coll, i))); };
var group2 = function(coll){ return (empty(coll) ? [] : cons(list(head(coll), at(coll, 1)), group2(from(coll, 2)))); };
var reverse = function(coll){ return fold(coll, function(acc, cur){ return cons(cur, acc); }, []); };
var comp = function(a, b){ return function(){ return a(app(b, toArray(arguments))); }; };
var str = function(){ return join(toArray(arguments), ""); };
var isArray = function(obj){ return (objStr(obj)==="[object Array]"); };
var isString = function(obj){ return (objStr(obj)==="[object String]"); };
var debug = function(str, val){ return (function(){
log(str);
return val;})(); };
var parens = list("(", ")");
var whitespace = list(" ", "\n");
var skipSpace = function(stri){ return (empty(stri) ? stri : (has(whitespace, head(stri)) ? skipSpace(tail(stri)) : stri)); };
var tokenRec = function(res, stri){ return (empty(stri) ? res : (function(){
var noSpaces = skipSpace(stri);
var hd = head(noSpaces);
var tl = tail(noSpaces);
return (has(parens, hd) ? tokenRec(push(res, hd), tl) : ((hd==="\"") ? nextOfString("", false, res, tl) : ((hd===";") ? nextOfComment(res, tl) : nextOfVal(hd, res, tl))));})()); };
var nextOfComment = function(res, stri){ return (empty(stri) ? res : (function(){
var hd = head(stri);
var tl = tail(stri);
return ((hd==='\n') ? tokenRec(res, tl) : nextOfComment(res, tl));})()); };
var nextOfVal = function(acc, res, stri){ return (empty(stri) ? push(res, acc) : (function(){
var hd = head(stri);
var tl = tail(stri);
return (has(whitespace, hd) ? tokenRec(push(res, acc), tl) : (has(parens, hd) ? tokenRec(push(res, acc), stri) : nextOfVal(str(acc, hd), res, tl)));})()); };
var nextOfString = function(acc, escape, res, stri){ return (empty(stri) ? err("next of string expected more characters") : (function(){
var hd = head(stri);
var tl = tail(stri);
return (((hd==="\\")&&!(escape)) ? nextOfString(str(acc, hd), true, res, tl) : (((hd==="\"")&&!(escape)) ? tokenRec(push(res, str("\"", acc, "\"")), tl) : nextOfString(str(acc, hd), false, res, tl)));})()); };
var tokenize = function(stri){ return tokenRec([], stri); };
var parse = function(tokens){ return parseRec([], tokens); };
var parseRec = function(tree, tokens){ return (empty(tokens) ? tree : (function(){
var hd = head(tokens);
var tl = tail(tokens);
return ((hd===")") ? err("error: did not expect closing parentheses") : ((hd==="(") ? parseStmt(1, push(tree, []), tl) : parseRec(push(tree, tokenToVal(hd)), tl)));})()); };
var pushFromBottom = function(coll, item, place){ return ((place===0) ? push(coll, item) : push(init(coll), pushFromBottom(last(coll), item, (place-1)))); };
var parseStmt = function(place, tree, tokens){ return (empty(tokens) ? err("error: expected more, unbalanced parentheses") : (function(){
var hd = head(tokens);
var tl = tail(tokens);
return ((hd===")") ? ((place===1) ? parseRec(tree, tl) : parseStmt((place-1), tree, tl)) : ((hd==="(") ? parseStmt((place+1), pushFromBottom(tree, [], place), tl) : parseStmt(place, pushFromBottom(tree, tokenToVal(hd), place), tl)));})()); };
var macros = object();
objSetExcl(macros, "!", function(tree){ return ((len(tree)===1) ? str("!(", compileExpr(head(tree)), ")") : err("! takes one arg")); });
var infix = list("+", "-", "/", "*", "&&", "||", "<", ">", "<=", ">=");
foreach(infix, function(infix){ return objSetExcl(macros, infix, function(tree){ return ((len(tree)===2) ? str("(", compileExpr(head(tree)), infix, compileExpr(at(tree, 1)), ")") : err(str(infix, " takes two args"))); }); });
objSetExcl(macros, "==", function(tree){ return ((len(tree)===2) ? str("(", compileExpr(head(tree)), "===", compileExpr(at(tree, 1)), ")") : err("== takes two args")); });
objSetExcl(macros, "!=", function(tree){ return ((len(tree)===2) ? str("(", compileExpr(head(tree)), "!==", compileExpr(at(tree, 1)), ")") : err("!= takes two args")); });
objSetExcl(macros, "mod", function(tree){ return ((len(tree)===2) ? str("(", compileExpr(head(tree)), "%", compileExpr(at(tree, 1)), ")") : err("mod takes two args")); });
objSetExcl(macros, "def", function(tree){ return ((len(tree)===2) ? str("var ", sanitize(unquote(head(tree))), " = ", compileExpr(at(tree, 1))) : err("def takes two args")); });
objSetExcl(macros, "if", function(tree){ return ((len(tree)===3) ? str("(", compileExpr(head(tree)), " ? ", compileExpr(at(tree, 1)), " : ", compileExpr(at(tree, 2)), ")") : err("if takes three args")); });
objSetExcl(macros, "do", function(tree){ return ((len(tree)===0) ? err("do takes at least one arg") : (function(){
var mapper = comp(function(s){ return str(s, ";\n"); }, compileExpr);
var mapped = map(init(tree), mapper);
var joined = join(mapped, "");
return str("(function(){\n", joined, "return ", compileExpr(last(tree)), ";})()");})()); });
objSetExcl(macros, "fn", function(tree){ return ((len(tree)===2) ? (function(){
var argsList = map(head(tree), unquote);
var joinedArgs = join(argsList, ", ");
var expr = compileExpr(at(tree, 1));
return str("function(", joinedArgs, "){ return ", expr, "; }");})() : err("fn takes two args")); });
objSetExcl(macros, "defn", function(tree){ return ((len(tree)===3) ? (function(){
var name = head(tree);
var args = at(tree, 1);
var body = at(tree, 2);
var anon = list(quote("fn"), args, body);
return compileExpr(list(quote("def"), name, anon));})() : err("defn takes three args")); });
objSetExcl(macros, "let", function(tree){ return ((len(tree)===0) ? err("let takes at least one arg") : (function(){
var expr = last(tree);
var body = map(init(tree), function(tree){ return cons(quote("def"), tree); });
var form = push(cons(quote("do"), body), expr);
return compileExpr(form, body);})()); });
objSetExcl(macros, "cond", function(tree){ return (((len(tree)%2)!==0) ? err("cond must take an even number of params") : ((len(tree)<=2) ? err("cond takes at least two args") : (function(){
var lt = last(tree);
var rest = slice(tree, 0, (len(tree)-2));
var grouped = group2(rest);
var form = foldR(grouped, function(acc, con){ return list(quote("if"), head(con), at(con, 1), acc); }, lt);
return compileExpr(form);})())); });
var tryMacros = function(key, tree){ return (hasKey(macros, key) ? (objGet(macros, key))(tail(tree)) : tree); };
var INT_REGEX = str("^", "(-)?", "[0-9]+", "(", "(?:E|e)", "[0-9]+)?", "$");
var STR_REGEX = str("^", "\"", ".*", "\"", "$");
var bools = list("true", "false");
var tokenToVal = function(token){ return (matches(token, INT_REGEX) ? token : (matches(token, STR_REGEX) ? token : (has(bools, token) ? token : quote(token)))); };
var quote = function(token){ return str("'", token); };
var unquote = function(token){ return slice(token, 1); };
var isQuoted = function(token){ return (head(token)==="'"); };
var compile = function(tree){ return (empty(tree) ? "" : str(compileExpr(head(tree)), ";\n", compile(tail(tree)))); };
var replExcl = function(name){ return replace(name, "!", "Excl"); };
var sanitize = function(name){ return (function(){
var indexDash = indexOf(name, "-");
return ((-1===indexDash) ? replExcl(name) : str(slice(name, 0, indexDash), upper(at(name, (indexDash+1))), sanitize(slice(name, (indexDash+2)))));})(); };
var compileExpr = function(expr){ return (isArray(expr) ? (function(){
var hd = head(expr);
var tl = tail(expr);
return (isString(hd) ? (function(){
var unquoted = unquote(hd);
return (hasKey(macros, unquoted) ? (objGet(macros, unquoted))(tl) : (function(){
var mapped = map(tl, compileExpr);
var joined = join(mapped, ", ");
return str(sanitize(unquoted), "(", joined, ")");})());})() : (function(){
var callee = compileExpr(hd);
var mapped = map(tl, compileExpr);
var joined = join(mapped, ", ");
return str("(", callee, ")(", joined, ")");})());})() : (isQuoted(expr) ? ((expr===quote("nil")) ? "[]" : unquote(sanitize(expr))) : expr)); };
var main = function(args){ return (function(){
var fs = require("fs");
var util = require("util");
var file = fs.readFileSync(at(args, 2), "utf-8");
var tokens = tokenize(file);
var parsed = parse(tokens);
var compiled = compile(parsed);
return log(compiled);})(); };
main(argv);

