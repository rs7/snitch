var express = require('express');
var router = express.Router();

router.get('/', function(req, res, next) {
    res.success(Date.now() / 1000 ^ 0);
});

module.exports = router;
