const cheerio = require('cheerio');
const request = require('request');

module.exports = getLastID;

function getLastID(cb) {
    execCatalog('catalog.php', cb);
}

function execCatalog(href, cb) {
    request('http://vk.com/' + href, function (err, res, body) {
        if (err) {
            cb(err);
            return;
        }

        const href = lastHref(body);

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
    const $ = cheerio.load(page);
    const href = $('#content').find('a').last().attr('href');
    return href;
}

function execLink(href, fcatalog, fid) {
    if (/^catalog\.php.*$/.test(href)) {
        fcatalog(href);
        return;
    }

    const match = /^id(\d+)$/.exec(href);

    if (!match) {
        throw new Error('Ссылка неизвестного типа: ' + href);
    }

    const id = match[1];
    fid(id);
}
