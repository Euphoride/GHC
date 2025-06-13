{-# LANGUAGE PatternSynonyms, ViewPatterns    #-}

{-|
This module defines the semi-ring of multiplicities, and associated functions.
Multiplicities annotate arrow types to indicate the linearity of the
arrow (in the sense of linear types).

Mult is a type synonym for Type, used only when its kind is Multiplicity.
To simplify dealing with multiplicities, functions such as
mkMultMul perform simplifications such as Many * x = Many on the fly.
-}
module GHC.Core.Multiplicity where

data MultiplicityFlag
  = RespectMultiplicities
  | IgnoreMultiplicities
