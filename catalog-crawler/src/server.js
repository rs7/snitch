var http = require('http');
var app = require('./app');

listenServer(http.createServer(app), 8081);

function listenServer(server, port) {
    server.listen(port, function () {
        var listen = server.address();
        console.log('listen ' + listen.address + ':' + listen.port);
    });
}
