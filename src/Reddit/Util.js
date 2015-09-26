// module Reddit.Util

exports.qsify = function (s) {
  return require('querystring').stringify(s);
}

exports.undefined = function() {
  throw new Error("undefined");
}
