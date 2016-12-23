'use strict';

const amqplib = require('amqplib');

const catalog = require('./catalog');

const QUEUE = 'snitch.catalog.request';

amqplib.connect('amqp://localhost').then(
    connection => connection.createChannel()
).then(
    channel => {
        const handler = message => messageHandler(channel, message);
        channel.assertQueue(QUEUE, {durable: false});
        channel.prefetch(1);
        channel.consume(QUEUE, handler);
    }
);

function messageHandler(channel, message) {
    const data = deserialize(message.content);

    const {type, id} = data;

    catalog.expand(type, id).then(response => {
        channel.assertQueue(message.properties.replyTo, {durable: false, expires: 60000});

        channel.sendToQueue(
            message.properties.replyTo,
            serialize(response),
            {correlationId: message.properties.correlationId}
        );

        channel.ack(message);
    }).catch(error => {
        channel.reject(message);
    });
}

function serialize(data) {
    return new Buffer(JSON.stringify(data));
}

function deserialize(buffer) {
    return JSON.parse(buffer.toString());
}