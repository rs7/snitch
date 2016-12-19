'use strict';

const cheerio = require('cheerio');

module.exports.selection = function selection(type, id) {
    let selection_1;
    let selection_2;
    let selection_3;

    id--;

    switch (type) {
        case 'e6':
            return [];

        case 'e4':
            selection_1 = id;
            return [selection_1];

        case 'e2':
            selection_1 = id / 100 ^ 0;
            selection_2 = id % 100;
            return [selection_1, selection_2];

        case 'e0':
            selection_1 = id / 10000 ^ 0;
            selection_2 = (id / 100 ^ 0) % 100;
            selection_3 = id % 100;
            return [selection_1, selection_2, selection_3];
    }
};

module.exports.query = function query(selection) {
    return selection.length == 0 ? null : `selection=${selection.join('-')}`;
};

module.exports.links = function links(html) {
    return cheerio.load(html)('#content').children('div.page_block').children('div').find('a').map(
        (index, element) => element.attribs.href
    ).get();
};

module.exports.link_to_id = function link_to_id(href, type, [selection1, selection2, selection3]) {
    switch (type) {
        case 'e6':
            return +href.slice(22) + 1;

        case 'e4':
            return +href.slice(23 + selection1.toString().length) + selection1 * 100 + 1;

        case 'e2':
            return +href.slice(24 + selection1.toString().length + selection2.toString().length) + selection1 * 10000 + selection2 * 100 + 1;

        case 'e0':
            return +href.slice(2);
    }
};
