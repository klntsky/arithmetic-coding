"use strict";
var Big = require('big.js');
Big.DP = 1e+6;

function wrap (f) {
    return function () {
        try {
            return f.apply(null, arguments);
        } catch (e) {
            return null;
        }
    };
};


function fn2 (property) {
    return function (a, b) {
        return a[property](b);
    };
};

function fn1 (property) {
    return function (a) {
        return a[property]();
    };
};

exports.fromStringI = wrap(function (str) {
    return new Big(str);
});

exports.toString = function (bd) {
    return bd.toFixed(1000).replace(/\.?0+$/, '...');
};

exports.toFixedI = function (b, n) {
    return b.toFixed(n).replace(/\.?0+$/, '');
};

exports.equals = function (a, b) {
    return !!a.eq(b);
};

exports.fromInt = function (int) {
    return new Big(int);
};

exports.compareTo = fn2('cmp');
exports.addI = fn2('plus');
exports.zeroI = new Big(0);
exports.mulI = fn2('times');
exports.oneI = new Big(1);
exports.subtract = fn2('minus');
exports.divideI = wrap(fn2('div'));
