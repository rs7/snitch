'use strict';

console.log('test');

const log = console.log.bind(console);

require('./request')(require('./query')()).then(body => {
    const links = require('./parse_links')(body);
    return links;
}).then(log).catch(log);
