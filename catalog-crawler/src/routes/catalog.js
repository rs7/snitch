var express = require('express');
var getHundred = require('./../controller/getHundred');
var getLast = require('./../controller/getLast');
var router = express.Router();

router.get('/last', function (req, res, next) {
    getLast(res.callback);
});

router.get('/hundred/:id', function (req, res, next) {
    var hundred = req.params.id;
    getHundred(hundred, res.callback);
});

module.exports = router;
