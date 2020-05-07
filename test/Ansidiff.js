const disparity = require("disparity")

exports._ansidiffLines = function(expected, actual) {
  var objs = disparity.unified(actual, expected);
  return objs
}
