const cheerio = require('cheerio');
const request = require('request');

module.exports = function (cb) {

    const link = 'http://vk.com/catalog.php';

    const href_slice_length = 22;

    request(link, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        const $ = cheerio.load(body);
        const list = $('#content').children('div.page_block').children('div').find('a').map(function () {
            return 1 + +$(this).attr('href').slice(href_slice_length);
        }).get();

        cb(null, list);
    });

};
