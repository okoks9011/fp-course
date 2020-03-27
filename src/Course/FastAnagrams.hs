{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import Data.List (sort)

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams target path = do
  contents <- readFile path
  let dic = lines contents
      sort' = listh . sort . hlist
      normalize = NoCaseString . sort' in
    pure $ intersectBy ((==) . normalize) dic (normalize target :. Nil)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
