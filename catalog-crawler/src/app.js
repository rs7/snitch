var bodyParser = require('body-parser');
var express = require('express');
var routes = require('./routes/index');

var app = express();

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: false}));

app.use(function (req, res, next) {
    res.success = function (response) {
        res.json({response: response})
    };
    res.fail = function (error) {
        res.json({error: error})
    };
    next();
});

app.use(routes);

app.use(function (req, res, next) {
    res.fail('Не найдено');
});

app.use(function (err, req, res, next) {
    res.fail(err);
});

module.exports = app;
