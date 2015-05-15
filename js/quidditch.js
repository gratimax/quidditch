// the original quidditch compiler in javascript
function tokenize(str) {
  return tokenRec([], str);
}

function tokenRec(res, str) {
  if (str.length === 0) {
    return res;
  } else {
    var s = skipSpace(str);
    var c = s.charAt(0);
    if (c == '(' || c == ')') {
      return tokenRec(res.concat([c]), s.substring(1));
    } else if (c == '"') {
      return nextOfString('', false, res, s.substring(1));
    } else if (c == ';') {
      return nextOfComment(res, s.substring(1));
    } else {
      return nextOfVal(c, res, s.substring(1));
    }
  }
}

function nextOfComment(res, str) {
  if (str.length == 0) {
    return res;
  } else {
    var c = str.charAt(0);
    if (c == '\n') {
      return tokenRec(res, str.substring(1));
    } else {
      return nextOfComment(res, str.substring(1));
    }
  }
}

function nextOfVal(acc, res, str) {
  if (str.length === 0) {
    return res.concat([acc]);
  } else {
    var c = str.charAt(0);
    if (c === ' ' || c === '\n') {
      return tokenRec(res.concat([acc]), str.substring(1));
    } else if (c == '(' || c == ')') {
      return tokenRec(res.concat([acc, c]), str.substring(1));
    } else {
      return nextOfVal(acc + c, res, str.substring(1));
    }
  }
}

function nextOfString(acc, escape, res, str) {
  if (str.length === 0) {
    console.log('error: next of string expected more string characters')
  } else {
    var c = str.charAt(0);
    if (c === '\\' && !escape) {
      return nextOfString(acc + c, true, res, str.substring(1));
    } else if (c === '"' && !escape) {
      return tokenRec(res.concat(['"' + acc + '"']), str.substring(1));
    } else {
      return nextOfString(acc + c, false, res, str.substring(1));
    }
  }
}

function skipSpace(str) {
  if (str.length === 0) {
    return str;
  } else {
    var c = str.charAt(0);
    if (c == ' ' || c == '\n') {
      return skipSpace(str.substring(1));
    } else {
      return str;
    }
  }
}

function compile(tokens) {
  if (tokens.length === 0) {
    return '';
  } else {
    var l = compileExpr(tokens);
    return l.str + ';\n' + compile(l.tokens);
  }
}

function compileExpr(tokens) {
  if (tokens.length === 0) {
    console.log('error: unexpected end');
  } else {
    var t = tokens[0];
    if (t === '(') {
      return compileApp(tokens.slice(1));
    } else if (t === ')') {
      console.log('error: unexpected closing paren expr');
    } else {
      if (t === 'nil') {
        return {str: '[]', tokens: tokens.slice(1)};
      } else {
        return {str: t, tokens: tokens.slice(1)};
      }
    }
  }
}

function compileApp(tokens) {
  if (tokens.length === 0) {
    console.log('error: unexpected end');
  } else {
    var t = tokens[0];
    if (t === '(') {
      var l = compileExpr(tokens);
      var l1 = compileArgs(l.tokens);
      return {str: l.str + '(' + l1.args.join(', ') + ')', tokens: l1.tokens};
    } else if (t === ')') {
      console.log('error: unexpected closing paren function call');
    } else {
      var tail = tokens.slice(1);
      if (t === 'def') {
        var l = compileArgs(tail);
        var args = l.args;
        if (args.length === 1) {
          return {str: 'var ' + args[0], tokens: l.tokens};
        } else if (args.length === 2) {
          return {str: 'var ' + args[0] + ' = ' + args[1], tokens: l.tokens};
        } else {
          console.log('def expects one or two args');
        }
      } else if (t === '==' || t === '+' || t === '-' || t === '/' || t === '*' || t === '&&' || t == '||') {
        var l1 = compileExpr(tail);
        var l2 = compileExpr(l1.tokens);
        var infix = (t === '==') ? '===' : t;
        return {str: '(' + l1.str + infix + l2.str + ')', tokens: l2.tokens.slice(1)};
      } else if (t === '!') {
        var l = compileExpr(tail);
        return {str: '!(' + l.str + ')', tokens: l.tokens.slice(1)}
      } else if (t === 'lam') {
        var a = compileLambdaArgs(tail.slice(1));
        var l = compileExpr(a.tokens);
        return {str: 'function(' + a.args.join(', ') + '){ return ' + l.str + '; }', tokens: l.tokens.slice(1)};
      } else if (t === 'if') {
        var cond = compileExpr(tail);
        var l1 = compileExpr(cond.tokens);
        var l2 = compileExpr(l1.tokens);
        return {str: '(' + cond.str + ' ? ' + l1.str + ' : ' + l2.str + ')', tokens: l2.tokens.slice(1)};
      } else if (t === 'do') {
        var l = compileDo([], tail);
        return {str: '(function(){ ' + l.stmts.join(';\n') + ';\nreturn ' + l.last + '; })()', tokens: l.tokens};
      } else {
        var l = compileArgs(tail);
        return {str: t + '(' + l.args.join(', ') + ')', tokens: l.tokens};
      }
    }
  }
}

function compileArgs(tokens) {
  if (tokens.length === 0) {
    console.log('error: unexpected end');
  } else {
    var t = tokens[0];
    if (t === ')') {
      return {args: [], tokens: tokens.slice(1)};
    } else {
      var l = compileExpr(tokens);
      var rest = compileArgs(l.tokens);
      return {args: [l.str].concat(rest.args), tokens: rest.tokens};
    }
  }
}

function compileLambdaArgs(tokens) {
  if (tokens.length === 0) {
    console.log('error: unexpected end');
  } else {
    var t = tokens[0];
    if (t === ')') {
      return {args: [], tokens: tokens.slice(1)};
    } else {
      var rest = compileLambdaArgs(tokens.slice(1));
      return {args: [t].concat(rest.args), tokens: rest.tokens};
    }
  }
}

function compileDo(stmts, tokens) {
  if (tokens.length === 0) {
    console.log('error: unexpected end');
  } else {
    var t = tokens[0];
    if (t === ')') {
      var stmtsInit = stmts.slice(0, stmts.length - 1);
      var stmtsLast = stmts[stmts.length - 1];
      return {stmts: stmtsInit, last: stmtsLast, tokens: tokens.slice(1)};
    } else {
      var l = compileExpr(tokens);
      return compileDo(stmts.concat(l.str), l.tokens);
    }
  }
}

console.log(compile(tokenize(require('fs').readFileSync(process.argv[2], 'utf-8'))));