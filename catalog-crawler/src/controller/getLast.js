var cheerio = require('cheerio');
var request = require('request');

function getLastID(cb) {
    execCatalog('catalog.php', cb);
}

function execCatalog(href, cb) {
    request('http://vk.com/' + href, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        var href = lastHref(body);

        execLink(href, fcatalog, fid);

        function fcatalog(href) {
            setTimeout(execCatalog, 0, href, cb);
        }

        function fid(id) {
            cb(null, id);
        }
    });
}

function lastHref(page) {
    var $ = cheerio.load(page);
    var href = $('#content').find('a').last().attr('href');
    return href;
}

function execLink(href, fcatalog, fid) {
    if (/^catalog\.php.*$/.test(href)) {
        fcatalog(href);
        return;
    }

    var match = /^id(\d+)$/.exec(href);

    if (!match) {
        throw new Error('Ссылка неизвестного типа: ' + href);
    }

    var id = match[1];
    fid(id);
}

module.exports = getLastID;
