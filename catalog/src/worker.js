'use strict';

const amqplib = require('amqplib');

const catalog = require('./catalog');

amqplib.connect('amqp://localhost').then(
    connection => connection.createChannel()
).then(
    channel => {
        const handler = message => messageHandler(channel, message);
        channel.prefetch(1);
        channel.consume('vk_catalog', handler);
    }
);

function messageHandler(channel, message) {
    const data = JSON.parse(message.content.toString());

    const {type, id} = data;

    catalog.expand(type, id).then(response => {
        channel.sendToQueue(
            message.properties.replyTo,
            new Buffer(JSON.stringify(response)),
            {correlationId: message.properties.correlationId}
        );

        channel.ack(message);
    }).catch(error => {
        channel.reject(message);
    });
}
