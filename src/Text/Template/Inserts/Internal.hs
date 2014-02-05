{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.Template.Inserts.Internal
-- Copyright   :  Joseph Abrahamson 2014
-- License     :  MIT
--
-- Maintainer  :  me@jspha.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- The internal workings of @inserts@. In most cases you don't want to be
-- digging around in this module, but it's useful if you want to somehow analyze
-- or transform the 'Template' type.
--
-- The usual caveat applies: this module is not a public API and is subject to
-- modification without warning.

module Text.Template.Inserts.Internal (

  -- * Major types
  Template (..), TemplateC (..), 

  -- ** The purely-Applicative 'Either'
  Got (..), gotEither,

  -- * Template functions
  runTemplate, showTemplate, parseTemplate, templateParser

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

-- | 'Template' chunks are either 'Literal's or 'Hole's to be filled by a 
-- runtime key lookup later.
data TemplateC = Literal Sb.Builder | Hole S.ByteString

instance Show TemplateC where
  show (Literal builder) =
    "Literal " ++ show (Sl.toStrict (Sb.toLazyByteString builder))
  show (Hole bs) = "Hole " ++ show bs

-- Templates are just 'DList's of template chunks. It might be wise to
-- replace the 'DList' with 'Data.Sequence.Sequence' and then keep everything in
-- the Haskell Platform. It'd also allow for a public helper function which
-- takes 'Data.Map.Map's directly.

newtype Template =
  Template { unTemplate :: Dl.DList TemplateC }

-- | /O(1)/ appends
instance Monoid Template where
  mempty = Template mempty
  Template t1 `mappend` Template t2 = Template (mappend t1 t2)

-- | 'Got' is the \"purely 'Applicative'\" 'Either' with 
-- @[S.ByteString]@ as its 'Left' type. When both the left and
-- right arguments to '(<*>)' are 'Miss' their errors are `mappend`ed
-- together.
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

-- | We can build a lazy 'Sl.ByteString' much more quickly, so if you need
-- to quickly show your templates then this might be nicer than using 'show'
-- directly.
showTemplate :: Template -> Sl.ByteString
showTemplate t =
  case runTemplate (\s -> Just $ "{{" <> s <> "}}") t of
    Left _  -> error "Impossible!"
    Right s -> s

instance Show Template where
  show = show . Sl.toStrict . showTemplate

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
