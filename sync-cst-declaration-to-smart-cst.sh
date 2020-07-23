#!/usr/bin/env bash

set -euxo pipefail

cp src/Language/PS/CST/Types/Declaration.purs src/Language/PS/SmartCST/Types/Declaration.purs

sed -i 's/Language.PS.CST.Types.Declaration/Language.PS.SmartCST.Types.Declaration/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/Language.PS.CST.Types.QualifiedName/Language.PS.SmartCST.Types.SmartQualifiedName\nimport Language.PS.SmartCST.Types.ConstructorProperName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/QualifiedName/SmartQualifiedName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/SmartSmartQualifiedName/SmartQualifiedName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/ProperName ProperNameType_ConstructorName/ConstructorProperName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/dataCtorName :: ConstructorProperName/dataCtorName :: ProperName ProperNameType_ConstructorName/g' src/Language/PS/SmartCST/Types/Declaration.purs
sed -i 's/name :: ConstructorProperName/name :: ProperName ProperNameType_ConstructorName/g' src/Language/PS/SmartCST/Types/Declaration.purs
