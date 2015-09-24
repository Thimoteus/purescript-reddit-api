// module Test.Main

exports.logAnything = function logAnything(x) {
  return function() {
    console.log(x);
    return {};
  }
}
