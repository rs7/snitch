'use strict';

const https = require('https');
const zlib = require('zlib');

const CONTENT_ENCODING = 'gzip';
const CONTENT_TYPE = 'text/html';

const OPTIONS = {
    host: 'vk.com',
    headers: {
        'Accept': CONTENT_TYPE,
        'Accept-Encoding': CONTENT_ENCODING,
        'Connection': 'close'
    },
    port: 443
};

module.exports = function request(query) {
    let path = '/catalog.php';

    if (query) {
        path += `?${query}`;
    }

    return new Promise((resolve, reject) => {
        const options = Object.assign({}, OPTIONS, {path});

        https.request(options, res => {
            const statusCode = res.statusCode;
            const contentType = res.headers['content-type'];
            const contentEncoding = res.headers['content-encoding'];

            if (statusCode !== 200) {
                reject(new Error(`Invalid status code ${statusCode}`));
                res.resume();
                return;
            }

            if (!new RegExp(`^${CONTENT_TYPE}`).test(contentType)) {
                reject(new Error(`Invalid content-type ${contentType}`));
                res.resume();
                return;
            }

            if (contentEncoding !== CONTENT_ENCODING) {
                reject(new Error(`Invalid content-encoding ${contentEncoding}`));
                res.resume();
                return;
            }

            let chunks = [];

            res.on('data', chunk => {
                chunks.push(chunk);
            });

            res.on('end', () => {
                zlib.gunzip(Buffer.concat(chunks), (error, decoded) => {
                    if (error) {
                        reject(error);
                        return;
                    }

                    resolve(decoded.toString());
                });
            });

        }).on('error', error => reject(error)).end();
    });
};
