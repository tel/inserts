{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}

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

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 ((.*>), (<*.))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as S
import qualified Data.ByteString.Builder          as Sb
import qualified Data.ByteString.Lazy             as Sl
import qualified Data.DList                       as Dl
import qualified Data.Foldable                    as F
import           Data.Monoid
import           Data.String

data TemplateC = Literal Sb.Builder | Hole S.ByteString

instance Show TemplateC where
  show (Literal builder) =
    "Literal " ++ show (Sl.toStrict (Sb.toLazyByteString builder))
  show (Hole bs) = "Hole " ++ show bs

newtype Template =
  Template { unTemplate :: Dl.DList TemplateC }

-- | /O(1)/ appends
instance Monoid Template where
  mempty = Template mempty
  Template t1 `mappend` Template t2 = Template (mappend t1 t2)

data Got a = Miss (Dl.DList S.ByteString) | Got a
  deriving Functor

instance Applicative Got where
  pure = Got
  Miss e1 <*> Miss e2 = Miss (e1 <> e2)
  Miss e  <*> _       = Miss e
  _       <*> Miss e  = Miss e
  Got f   <*> Got x   = Got (f x)

gotEither :: Got a -> Either [S.ByteString] a
gotEither (Miss e) = Left (Dl.toList e)
gotEither (Got  a) = Right a

instance Monoid a => Monoid (Got a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Outputs either the successfully interpolated template or the list of
-- missing keys. For fast operation, try building the lookup function using
-- @unordered-containers@ @HashMap@s.
runTemplate
  :: (S.ByteString -> Maybe S.ByteString)
  -> Template -> Either [S.ByteString] Sl.ByteString
runTemplate lookMay =
  gotEither . fmap Sb.toLazyByteString
            . F.foldMap get
            . unTemplate
  where
    get (Literal b) = pure b
    get (Hole name) = Sb.byteString <$> look name
    look :: S.ByteString -> Got S.ByteString
    look s = maybe (Miss (pure s)) Got (lookMay s)

showTemplate :: Template -> S.ByteString
showTemplate t =
  case runTemplate (\s -> Just $ "{{" <> s <> "}}") t of
    Left _  -> error "Impossible!"
    Right s -> Sl.toStrict s

instance Show Template where
  show = show . showTemplate

-- | Try to parse a 'S.ByteString' as a 'Template'.
parseTemplate :: S.ByteString -> Either String Template
parseTemplate = A.parseOnly templateParser

-- | Template literals can be embedded directly in Haskell files.
instance IsString Template where
  fromString s =
    case parseTemplate (fromString s) of
      Right a -> a
      Left _  -> error ("Could not parse a Template: " ++ show s)

foldlM :: MonadPlus f => (b -> a -> b) -> b -> f a -> f b
foldlM mix seed gen = do
  may <- liftM Just gen `mplus` return Nothing
  case may of
    Nothing -> return seed
    Just a  -> foldlM mix (mix seed a) gen

foldMonoidM :: (MonadPlus f, Monoid b) => (a -> b) -> f a -> f b
foldMonoidM f = foldlM (\b a -> b <> f a) mempty

-- | An @attoparsec@ 'A.Parser' for 'Template's. This is useful if you'd
-- like to embed 'Template's into a more sophisticated, parseable type of
-- your own.
templateParser :: A.Parser Template
templateParser = foldMonoidM (Template . pure) templateChunk
  where
    templateChunk :: A.Parser TemplateC
    templateChunk =
      A.choice [ hole, noBraces ]

    noBraces :: A.Parser TemplateC
    noBraces =
      Literal . Sb.byteString <$> A.takeWhile1 (not . (== '{'))

    singleBrace :: A.Parser TemplateC
    singleBrace =
      let build c = Literal (Sb.char8 '{' <> Sb.char8 c)
      in  build <$> A.try (A.char '{' *> A.satisfy (not . (== '{')))

    hole :: A.Parser TemplateC
    hole =
      "{{" .*> A.skipSpace *>
      (Hole <$> A.takeWhile1 (\c -> not (A.isSpace c || c == '}')))
      <* A.skipSpace <*. "}}"
