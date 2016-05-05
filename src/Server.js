"use strict";

// module Server


exports.ServerStart = ServerStart


var http = require('http');
var url = require('url');
var util = require('util');
function ServerStart(port, f)
{
    http.createServer(function (request, response){
	
	response.writeHead(200,{'Content-Type': 'text/html'});

	response.end(f(request.url));
    }).listen(port);
    return 0;

}

