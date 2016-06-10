var express = require('express');
var router = express.Router();

router.use('/catalog', require('./catalog'));
router.use('/time', require('./time'));

module.exports = router;
