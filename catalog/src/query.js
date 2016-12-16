'use strict';

const cheerio = require('cheerio');

function query(type, id) {
    return type ? `selection=${selection(type, id)}` : null;
}

function selection(type, id) {
    let selection_1;
    let selection_2;
    let selection_3;

    id--;

    switch (type) {
        case 'e6':
            selection_1 = id;
            return [selection_1];

        case 'e4':
            selection_1 = id / 100 ^ 0;
            selection_2 = id % 100;
            return [selection_1, selection_2];

        case 'e2':
            selection_1 = id / 10000 ^ 0;
            selection_2 = (id / 100 ^ 0) % 100;
            selection_3 = id % 100;
            return [selection_1, selection_2, selection_3];
    }
}

function parse_data(data) {
    return cheerio.load(data)('#content').children('div.page_block').children('div').find('a').map(
        (index, element) => element.attribs.href
    ).get();
}

function parse_href(href, type, [selection1, selection2, selection3]) {
    switch (type) {
        case 'e6':
            return href.slice(22);

        case 'e4':
            return href.slice(23 + selection1.toString().length);

        case 'e2':
            return href.slice(24 + selection1.toString().length + selection2.toString().length);

        case 'e0':
            return href.slice(2);
    }
}

//+href.slice(href_slice_length) + 1;
//+href.slice(href_slice_length) + selection1E6 * 100 + 1;
//+href.slice(href_slice_length) + selection1E6 * 10000 + selection1E4 * 100 + 1;
//+href.slice(href_slice_length);
