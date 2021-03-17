{-# LANGUAGE LambdaCase #-}

{-|
Module: TIE.Override

Contains the functions for user-defined overriding of generated types.
-}
module TIE.Override (overrideMembers) where

import           TIE.Config     (Override (..))
import           TIE.TypeScript (AliasName (AliasName), Interface (Interface),
                                 InterfaceName (InterfaceName), Member (..),
                                 NamespaceMember (NMAlias, NMInterface),
                                 PropertyName (PropertyName))

type Warnings = [Text]
type OverrideMessages = [Text]

{-|
  Returns a modified array of namespace members, with any matching overrides applied to their types.

  For each successful override performed, an OverrideMessage will also be emitted which can be printed
  to the screen to inform the user that the override was successful.

  For each override which couldn't be performed, a warning will be emitted to explain why it wasn't successful.
-}
overrideMembers :: [NamespaceMember] -> [Override] -> ([NamespaceMember], Warnings, OverrideMessages)
overrideMembers = go [] []
  where
    go :: [Text] -> OverrideMessages -> [NamespaceMember] -> [Override] -> ([NamespaceMember], Warnings, OverrideMessages)
    go ws oMessages ms [] = (ms, ws, oMessages)
    go ws oMessages ms (o:os) = case ovrPath o of
      (n1 :| [n2]) -> case find (isTopLevelMatch n1) ms of
        Just (NMInterface (Interface exported _ iMembers) ) -> case find (isPropertyMatch n2) iMembers of
          Just (MProperty _ _) ->
            go
              ws
              ("Overriding " <> n1 <> "." <> n2 : oMessages)
              (NMInterface
                (Interface exported (InterfaceName n1)
                  (MProperty (PropertyName n2) (ovrType o) : filter (not . isPropertyMatch n2) iMembers))
                : filter (not. isTopLevelMatch n1) ms)
              os
          Just (MFunction _) ->
            go (couldNotOverrideWarning o "you can't override the type of a function" : ws) oMessages ms os
          Just (MPropertyGroup _ _) ->
            go (couldNotOverrideWarning o "you can't override the type of a group of properties" : ws) oMessages ms os
          Nothing ->
            go (couldNotOverrideWarning o (n1 <> " doesn't have a property called " <> n2) : ws) oMessages ms os
        Just (NMAlias _ _) ->
          go (couldNotOverrideWarning o (n1 <> " is an alias, and aliases don't have properties") : ws) oMessages ms os
        _ ->
          go (couldNotOverrideWarning o doesntExist : ws) oMessages ms os
      (n1 :| []) -> case find (isTopLevelMatch n1) ms of
        Just (NMAlias _ _) ->
          go ws ("Overriding " <> n1 : oMessages) (NMAlias (AliasName n1) (ovrType o) : filter (not . isTopLevelMatch n1) ms) os
        Just (NMInterface _) ->
          go (couldNotOverrideWarning o "it's an interface. You can override individual properties on it though" : ws) oMessages ms os
        _ ->
          go (couldNotOverrideWarning o doesntExist : ws) oMessages ms os
      _ -> go (couldNotOverrideWarning o doesntExist : ws) oMessages ms os

isTopLevelMatch :: Text -> NamespaceMember -> Bool
isTopLevelMatch name = \case
  NMInterface (Interface _ (InterfaceName iName) _) ->
    name == iName
  NMAlias (AliasName aName) _ ->
    name == aName
  _ ->
    False

isPropertyMatch :: Text -> Member -> Bool
isPropertyMatch name = \case
  MProperty (PropertyName pName) _ -> name == pName
  _                                -> False

couldNotOverrideWarning :: Override -> Text -> Text
couldNotOverrideWarning o reason = "Could not override " <> (mconcat . intersperse "." . toList $ ovrPath o) <> " - " <> reason <> "."

doesntExist :: Text
doesntExist = "it doesn't exist"

