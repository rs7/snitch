var http = require('http');
var url = require('url');
var querystring = require('querystring');
var zlib = require('zlib');

var count = 0;

function accept(req, res) {

    count++;

    var response = JSON.stringify({response: Date.now() / 1000 | 0})

    console.log(`${count} ${response}`);

    //zlib.gzipSync(response);

    res.writeHead(200, {
        'Cache-Control': 'no-store',
        'Connection': 'keep-alive',
        //'Content-Encoding': 'gzip',
        'Content-Length': Buffer.byteLength(response),
        'Content-Type': 'application/json; charset=utf-8',
        'Date': 'Fri, 11 Nov 2016 13:59:31 GMT',
        'Pragma': 'no-cache',
        'Server': 'Apache',
        'X-Powered-By': 'PHP/3.7216'
    });

    res.end(response);

    if (count == 100) {
        req.socket.destroy();
        count = 0;
    }
}

const server = http.createServer(accept).listen(80);
