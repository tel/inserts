
-- |
-- Module      :  Text.Template.Inserts
-- Copyright   :  Joseph Abrahamson 2014
-- License     :  MIT
--
-- Maintainer  :  me@jspha.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Completely trivial, interpolation-only 'Template's; for when you want an API
-- that fits on a business card. "Text.Template.Inserts" implements a
-- subset of Mustache syntax. It uses template strings with named holes
-- deliminted by \"mustaches\":
--
-- > import Data.HashMap.Strict as Map
-- > import Data.ByteString     as S
-- >
-- > context :: HashMap ByteString ByteString
-- > context = Map.fromList [ ("country", "Morocco")
-- >                        , ("favoriteFruit", "bananas")
-- >                        ]
--
-- >>> runTemplate (flip Map.lookup context) "I live in {{country}} and love {{favoriteFruit}}."
-- Right "I live in Morocco and love bananas"
--
-- >>> runTemplate (flip Map.lookup context) "My address is {{ address }}"
-- Left ["address"]
--
-- "Text.Template.Inserts" seeks to be as unsurprising and simple as
-- possible sacrificing all kinds of niceities. Sometimes though, all you
-- need is obvious, trivial string interpolation

module Text.Template.Inserts (

  Template, runTemplate, parseTemplate, templateParser

) where

import Text.Template.Inserts.Internal
