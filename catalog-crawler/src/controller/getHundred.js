var cheerio = require('cheerio');
var request = require('request');

function buildLink(hundred) {
    var sectionC = hundred % 100;
    var sectionB = (hundred / 100 ^ 0) % 100;
    var sectionA = hundred / 10000 ^ 0;

    return 'http://vk.com/catalog.php?selection=' + sectionA + '-' + sectionB + '-' + sectionC;
}

function parseIDs(page) {
    var $ = cheerio.load(page);
    var list = $('#content').find('a').map(function () {
        return +$(this).attr('href').slice(2);
    }).get();
    return list;
}

function getHundred(hundred, cb) {
    var link = buildLink(hundred);

    request(link, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        var list = parseIDs(body);
        cb(null, list);
    });
}

module.exports = getHundred;
