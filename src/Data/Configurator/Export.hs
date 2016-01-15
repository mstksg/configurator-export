{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Configurator.Export (
  -- * Pretty printing
    renderConf
  , displayConf
  , renderHashMap
  -- ** Write to file
  , writeConf
  -- * Doc
  , confDoc
  , hashMapDoc
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Configurator
import Data.Configurator.Types
import Data.Foldable
import Data.Function
import Data.HashMap.Strict           (HashMap)
import Data.List
import Data.List.NonEmpty            (NonEmpty(..))
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Text                     (Text)
import Text.PrettyPrint              (Doc, (<+>), ($+$))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Text           as T
import qualified Text.PrettyPrint    as P

data HashMapTree k v = HMT { getHashMapTree :: HashMap k (Either v (HashMapTree k v))
                           }
  deriving (Show, Eq)

renderConf :: Config -> IO String
renderConf c = renderHashMap <$> getMap c

renderHashMap :: HashMap Name Value -> String
renderHashMap = P.render . hashMapDoc

displayConf :: Config -> IO ()
displayConf c = putStrLn =<< renderConf c

writeConf :: FilePath -> Config -> IO ()
writeConf fp = writeFile fp <=< renderConf

hashMapDoc :: HashMap Name Value -> P.Doc
hashMapDoc = hmtToDoc . fromHashMap "."

confDoc :: Config -> IO P.Doc
confDoc c = hashMapDoc <$> getMap c

fromHashMap
    :: forall v.
       Text
    -> HashMap Text v
    -> HashMapTree Text v
fromHashMap sep = go
  where
    go :: HashMap Text v -> HashMapTree Text v
    go = HMT
       . HM.fromList
       . map condense
       . NE.groupAllWith (NE.head . fst)
       . (map . first) (NE.fromList . T.splitOn sep)
       . HM.toList
    condense :: NonEmpty (NonEmpty Text, v) -> (Text, Either v (HashMapTree Text v))
    condense kvs@((topKey :| subKeys, v1) :| _) = (topKey, hm)
      where
        hm =  case subKeys of
                [] -> Left v1           -- kvs' is [], only one value
                _  -> Right
                    . go
                    . HM.fromList
                    . (map . first) (T.intercalate sep . NE.tail)
                    $ toList kvs

hmtToDoc :: HashMapTree Text Value -> P.Doc
hmtToDoc = P.vcat
         . map (uncurry go)
         . sortBy ((biasRight `on` snd) <> comparing fst)
         . HM.toList
         . getHashMapTree
  where
    biasRight :: Either a b -> Either a b -> Ordering
    biasRight (Right _) (Right _) = EQ
    biasRight (Right _) _         = LT
    biasRight (Left  _) (Left  _) = EQ
    biasRight (Left  _) _         = GT
    go :: Text -> Either Value (HashMapTree Text Value) -> P.Doc
    go k0 e =
      case e of
        Left v  -> P.hsep [P.text (T.unpack k0), P.equals, valueToDoc v]
        Right t -> P.text (T.unpack k0) <+> P.lbrace
               $+$ P.nest 4 (hmtToDoc t)
               $+$ P.rbrace

valueToDoc :: Value -> Doc
valueToDoc (Bool b)   = bool "false" "true" b
valueToDoc (String t) = P.text $ show t
valueToDoc (Number n) | denominator n == 1 = P.integer $ round n
                      | otherwise          = P.double  $ fromRational n
valueToDoc (List vs)  = P.brackets
                      . P.hsep
                      . P.punctuate P.comma
                      $ map valueToDoc vs


