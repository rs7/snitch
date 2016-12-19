'use strict';

const lib = require('./lib');
const request = require('./request');

module.exports.expand = function expand(type, id) {
    const selection = lib.selection(type, id);
    const query = lib.query(selection);

    return request(query).then(html => {
        const links = lib.links(html);
        const ids = links.map(link => lib.link_to_id(link, type, selection));
        return ids;
    });
};
