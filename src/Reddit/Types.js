// module Reddit.Types

exports.qsify = function (s) {
  return require('querystring').stringify(s);
}
