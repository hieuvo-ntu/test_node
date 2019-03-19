//import http from 'http'
var http = require('http');
var fs   = require('fs');
console.log(__dirname+'âssa');

http.createServer(function(req, res){
    res.writeHead('200',{'Content-Type':'text/html'})
    res.end('<h1>Welcome to website</h1>')
}).listen(8000) 

// function Tong(a, b){
//     this.a = a;
//     this.b = b;
// }
// Tong.prototype.add = function (){
//     console.log(this.a+this.b);
// }
//  var b = new Tong(2,4)
//  b.add();