"use strict";

// module Server


exports.serverStart = serverStart


var http = require('http');
var url = require('url');
var util = require('util');
function serverStart(port, f)
{
    http.createServer(function (request, response){
	
	response.writeHead(200,{'Content-Type': 'text/html'});

	response.end(f(request.url));
    }).listen(port);
    return 0;

}

