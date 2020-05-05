const ansidiff = require("ansidiff")

exports._ansidiffLines = function(actual, expected) {
  return ansidiff.lines(actual, expected)
}
