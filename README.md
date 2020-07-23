# purescript-ps-cst

### Objectives:
- types are a copy of purescript-cst (on [hackage](https://hackage.haskell.org/package/purescript/docs/Language-PureScript-CST-Types.html), [github](https://github.com/purescript/purescript/blob/master/lib/purescript-cst))
- don't aim to be a complete copy of purescript-cst (because these types were created for parsing, not printing), but support 80% of most used printing functionality
- parentheses are added automatically, there is no need to use `ExprParens` or `TypeParens` (which are added by purescript parser on parsing phase)

### TODO:

- [ ] use https://pursuit.purescript.org/packages/purescript-optparse/3.0.0/docs/Text.PrettyPrint.Leijen instead of Boxes to allow for example:

   - configuring width of printed document
   - conditionally print `Type`'s in one line or in multiple lines based on expected line width (and not based on context as it is now)

- [ ] support comments
- [x] calculate imports automatically based on `Declaration`s
- [ ] finish pending tests in `spec` directory
- [ ] provide configuration for printing function (e.g. allow to use `∀` instead of `forall`)
