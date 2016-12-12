const cheerio = require('cheerio');
const request = require('request');

module.exports = function (hundred_id, cb) {

    const selection_1 = (hundred_id - 1) / 10000 ^ 0;
    const selection_2 = ((hundred_id - 1) / 100 ^ 0) % 100;
    const selection_3 = (hundred_id - 1) % 100;

    const link = `http://vk.com/catalog.php?selection=${selection_1}-${selection_2}-${selection_3}`;

    const href_slice_length = 2;

    request(link, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        const $ = cheerio.load(body);
        const list = $('#content').children('div.page_block').children('div').find('a').map(function () {
            return +$(this).attr('href').slice(href_slice_length);
        }).get();

        cb(null, list);
    });

};
