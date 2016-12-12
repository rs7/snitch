const http = require('http');
const app = require('./app');

listenServer(http.createServer(app), 8081);

function listenServer(server, port) {
    server.listen(port, function () {
        const listen = server.address();
        console.log('listen ' + listen.address + ':' + listen.port);
    });
}
