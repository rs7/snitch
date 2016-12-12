const bodyParser = require('body-parser');
const express = require('express');
const routes = require('./routes/index');

const app = express();

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: false}));

app.use(function (req, res, next) {
    res.success = function (response) {
        res.json({response: response})
    };
    res.fail = function (error) {
        res.json({error: error})
    };
    res.callback = function (error, result) {
        if (error) {
            this.fail(error);
            return;
        }
        this.success(result);
    }.bind(res);
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
