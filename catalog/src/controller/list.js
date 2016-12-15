const cheerio = require('cheerio');
const request = require('request');

module.exports = getList;

const catalogRequest = request.defaults({
    baseUrl: 'http://vk.com/catalog.php',
    gzip: true
});

function doRequest(query, callback) {
    catalogRequest(query, requestCallback);

    function requestCallback(error, response, body) {
        if (error) {
            callback(error);
            return;
        }

        const list = cheerio.load(body)('#content').children('div.page_block').children('div').find('a').map(
            function () {
                return $(this).attr('href');
            }
        ).get();

        callback(null, list);
    }

}

const Types = Object.freeze({E6: 0, E4: 1, E2: 2, E0: 3});

function getList(type, id, callback) {

}

function getE6(callback) {
    doRequest(null, function (error, result) {
        if (error) {
            callback(error);
            return;
        }


    })
}

let [selection1E6, selection1E4, selection1E2] = selection;

    const selection1E6Part = selection1E6 ? `?selection=${selection1E6}` : '';
    const selection1E4Part = selection1E4 ? `-${selection1E4}` : '';
    const selection1E2Part = selection1E2 ? `-${selection1E2}` : '';

    const query = `${selection1E6Part}${selection1E4Part}${selection1E2Part}`;

    const href_slice_length = [
        22,
        23 + selection1E6.toString().length,
        24 + selection1E6.toString().length + selection1E4.toString().length,
        2
    ][selection.length];

    //+href.slice(href_slice_length) + 1;
    //+href.slice(href_slice_length) + selection1E6 * 100 + 1;
    //+href.slice(href_slice_length) + selection1E6 * 10000 + selection1E4 * 100 + 1;
    //+href.slice(href_slice_length);
