const cheerio = require('cheerio');
const request = require('request');

module.exports = function (ten_thousand_id, cb) {

    const selection_1 = (ten_thousand_id - 1) / 100 ^ 0;
    const selection_2 = (ten_thousand_id - 1) % 100;

    const link = `http://vk.com/catalog.php?selection=${selection_1}-${selection_2}`;

    const href_slice_length = 24 + selection_1.toString().length + selection_2.toString().length;

    request(link, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        const $ = cheerio.load(body);
        const list = $('#content').children('div.page_block').children('div').find('a').map(function () {
            return 1 + +$(this).attr('href').slice(href_slice_length) + selection_1 * 10000 + selection_2 * 100;
        }).get();

        cb(null, list);
    });

};
