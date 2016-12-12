const express = require('express');
const router = express.Router();

router.use('/catalog', require('./catalog'));
router.use('/time', require('./time'));

module.exports = router;
