const net = require('net');

const request = "GET /method/utils.getServerTime HTTP/1.1\nHost: api.vk.com\n\n";

const data = request.repeat(100);

var count = process.argv[2] || 1;

function tick() {
    var client = new net.Socket();
    client.connect(80, 'api.vk.com', function () {
        console.log('Connected');

        client.write(data, function () {
            console.log('Writed');
        });
    });

    client.on('data', function (data) {
        console.log('Received: ' + data.length);
    });

    client.on('close', function () {
        console.log('Connection closed', count);

        if (--count) {
            setTimeout(tick, 0);
        }
    });
}

tick();
