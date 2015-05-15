var s1="hello how are you";
var s2="testing \n out \\ escapes \" \\\ ";
var s3="\\";
var join=function(coll, s){ return coll.join(s); };
var toArray=function(coll){ return Array.prototype.slice.call(coll); };
var list=function(){ return toArray(arguments); };
var str=function(){ return join(toArray(arguments), ""); };
console.log(list(1, 2, 3, 4));
console.log(s2);

