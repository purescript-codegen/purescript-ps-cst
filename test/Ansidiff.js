// from https://github.com/trentm/node-ansidiff/blob/master/lib/ansidiff.js

const diff = require("diff")

function bright(obj) {
  if (obj.added) {
    return (
      '\033[' + 7 + 'm'   // inverse
      + '\033[' + 32 + 'm'  // green
      + obj.value
      + '\033[' + 39 + 'm'
      + '\033[' + 27 + 'm'
    );
  } else if (obj.removed) {
    return (
      '\033[' + 7 + 'm'     // inverse
      + '\033[' + 31 + 'm'  // red
      + obj.value
      + '\033[' + 39 + 'm'
      + '\033[' + 27 + 'm'
    );
  } else {
    return obj.value;
  }
}

exports._ansidiffLines = function(actual, expected) {
  var objs = diff.diffLines(actual, expected, { ignoreWhitespace: false });
  return objs.map(bright).join('');
}
