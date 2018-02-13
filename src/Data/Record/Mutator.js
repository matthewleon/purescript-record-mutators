"use strict";

// stolen from purescript-record: https://github.com/purescript/purescript-record
exports.copyRecord = function(rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

// stolen from purescript-record: https://github.com/purescript/purescript-record
exports.unsafeSet = function(l) {
  return function(a) {
    return function(rec) {
      rec[l] = a;
      return rec;
    };
  };
};

exports.unsafeModify = function(l) {
  return function(f) {
    return function(rec) {
      rec[l] = f(rec[l]);
      return rec;
    };
  };
};
