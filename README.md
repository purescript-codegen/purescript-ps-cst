# purescript-ps-cst

TODO:

- [ ] use https://pursuit.purescript.org/packages/purescript-optparse/3.0.0/docs/Text.PrettyPrint.Leijen instead of Boxes
- [ ] support comments
- [ ] calculate imports automatically based on `Declaration`s
- [ ] finish pending tests in `spec` directory
- [ ] give user to provide configuration for printing function (e.g. use `∀` instead of `forall`)

Objectives:
- types are a copy of purescript-cst (on [hackage](https://hackage.haskell.org/package/purescript/docs/Language-PureScript-CST-Types.html), [repository directory](https://github.com/purescript/purescript/blob/master/lib/purescript-cst))
- don't aim to be a complete copy of purescript-cst (because these types were created for parsing, not printing), but support 80% of most used printing functionality
- parentheses are added automatically, there is no need to use `ExprParens` or `TypeParens` (which are added by purescript parser on parsing phase)
