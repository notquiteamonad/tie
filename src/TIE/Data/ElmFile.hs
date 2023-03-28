module TIE.Data.ElmFile where

import qualified Language.Elm.Name as ElmName
import qualified Language.Elm.Type as ElmType

newtype RawElmFile = RawElmFile Text

data ElmFile v = ElmFile
  { elmFileExports :: [ElmType.Type v],
    elmFileImports :: [Import]
  }

data Import
  = ImportQualified ElmName.Module
  | ImportQualifiedAlias ElmName.Module ElmName.Local
  | ImportAll ElmName.Module
  | ImportSome ElmName.Module [ImportSelection]

data ImportSelection
  = ImportSelection ElmName.Local
  | ImportSelectionWithConstructors ElmName.Local
