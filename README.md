# purescript-ps-ast

Currently this package provides an opinionated AST for PureScript which was ripped from purescript-react-basic-mui codegen tool. We hope to turn it into somethig more generic and complete.

TODO:
- what algebra allows to peek at next child? prettify doWrap
- use https://pursuit.purescript.org/packages/purescript-optparse/3.0.0/docs/Text.PrettyPrint.Leijen instead of Boxes

Objectives:
- types are copy of purescript-cst
- parentheses are added automatically, there is no need to use ExprParens or TypeParens (they are added by purescript parser on parsing phase)
