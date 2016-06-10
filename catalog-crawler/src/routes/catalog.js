var express = require('express');
var getLast = require('./../controller/getLast');
var router = express.Router();

router.get('/last', function (req, res, next) {
    getLast(res.callback);
});

module.exports = router;
