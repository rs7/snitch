const express = require('express');

const getMillions = require('./../controller/getMillions');
const getTenThousands = require('./../controller/getTenThousands');
const getHundreds = require('./../controller/getHundreds');
const getOnes = require('./../controller/getOnes');

const getLast = require('./../controller/last');

const router = express.Router();

router.get('/last', function (req, res, next) {
    getLast(res.callback);
});

router.get('/millions', function (req, res, next) {
    getMillions(res.callback);
});

router.get('/ten_thousands/:id', function (req, res, next) {
    getTenThousands(req.params.id, res.callback);
});

router.get('/hundreds/:id', function (req, res, next) {
    getHundreds(req.params.id, res.callback);
});

router.get('/ones/:id', function (req, res, next) {
    getOnes(req.params.id, res.callback);
});

module.exports = router;
