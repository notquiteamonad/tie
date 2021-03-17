module TIE.OverrideSpec (spec) where

import           TIE.Config     (Override (Override))
import           TIE.Override   (overrideMembers)
import           TIE.TypeScript (AliasName (AliasName), Exported (Exported),
                                 Interface (Interface),
                                 InterfaceName (InterfaceName),
                                 Member (MProperty),
                                 NamespaceMember (NMAlias, NMInterface),
                                 PrimitiveName (PNull),
                                 PropertyName (PropertyName),
                                 TSType (TLiteral, TPrimitive))
import           Test.Hspec     (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "overrideMembers" do
    it "overrides the appropriate properties" do
      overrideMembers
        [ NMAlias (AliasName "A1") (TPrimitive PNull)
        , NMInterface (Interface Exported (InterfaceName "I1")
            [ MProperty (PropertyName "p1") (TPrimitive PNull)
            ]
          )
        ]
        [ Override ("I1" :| []) (TLiteral "foo")
        , Override ("I1" :| ["p1"]) (TLiteral "bar")
        , Override ("A1" :| ["p1"]) (TLiteral "baz")
        , Override ("A1" :| []) (TLiteral "qux")
        ]
        `shouldBe`
        ( [ NMAlias (AliasName "A1") (TLiteral "qux")
          , NMInterface (Interface Exported (InterfaceName "I1")
              [ MProperty (PropertyName "p1") (TLiteral "bar")
              ]
            )
          ]
        , [ "Could not override A1.p1 - A1 is an alias, and aliases don't have properties."
          , "Could not override I1 - it's an interface. You can override individual properties on it though."
          ]
        , ["Overriding A1", "Overriding I1.p1"]
        )
