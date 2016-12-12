const cheerio = require('cheerio');
const request = require('request');

module.exports = function (million_id, cb) {

    const selection_1 = million_id - 1;

    const link = `http://vk.com/catalog.php?selection=${selection_1}`;

    const href_slice_length = 23 + selection_1.toString().length;

    request(link, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        const $ = cheerio.load(body);
        const list = $('#content').children('div.page_block').children('div').find('a').map(function () {
            console.log($(this).attr('href'));
            console.log($(this).attr('href').slice(href_slice_length));
            console.log(+$(this).attr('href').slice(href_slice_length));
            console.log('\n');
            return 1 + +$(this).attr('href').slice(href_slice_length) + selection_1 * 100;
        }).get();

        cb(null, list);
    });

};
