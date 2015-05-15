var fs = require("fs");
var slurp = function(file){ return fs.readFileSync(file, "utf-8"); };
var log = function(str){ return console.log(str); };
log(slurp("quidditch.qd"));

