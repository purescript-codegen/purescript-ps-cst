#!/usr/bin/env bash

set -euxo pipefail

cp src/Language/PS/CST/Types/Declaration.purs src/Language/PS/SmartCST/Types/Declaration.purs

# update import
sed -i 's/Language.PS.CST.Types.Declaration/Language.PS.SmartCST.Types.Declaration/g' src/Language/PS/SmartCST/Types/Declaration.purs

# update import and add 1 import
sed -i 's/import Language.PS.CST.Types.QualifiedName (QualifiedName)/import Language.PS.SmartCST.Types.SmartQualifiedName (SmartQualifiedName)\nimport Language.PS.SmartCST.Types.SmartQualifiedNameConstructor (SmartQualifiedNameConstructor)/g' src/Language/PS/SmartCST/Types/Declaration.purs

# replace
sed -i 's/QualifiedName (ProperName ProperNameType_ConstructorName)/SmartQualifiedNameConstructor/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/QualifiedName/SmartQualifiedName/g' src/Language/PS/SmartCST/Types/Declaration.purs

# fix result of prev replaces
sed -i 's/SmartSmartQualifiedName/SmartQualifiedName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/SmartSmartQualifiedNameConstructor/SmartQualifiedNameConstructor/g' src/Language/PS/SmartCST/Types/Declaration.purs

# add
sed -i 's/ExprIdent (SmartQualifiedName Ident)/ExprIdent (SmartQualifiedName Ident)\n  | ExprVar Ident -- like ExprIdent, but without import/g' src/Language/PS/SmartCST/Types/Declaration.purs
