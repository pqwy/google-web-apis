{-# OPTIONS_GHC -Wall #-}

module Web.Google.Utilities (
      liftIteratee, liftIterateeIO, addHeaders , addQuery, parseUrl'
) where

import           Data.Maybe (fromJust)
import           Control.Arrow (first)

import qualified Data.CaseInsensitive as CI (mk)

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Enumerator hiding (map)

import           Network.HTTP.Types
import           Network.HTTP.Enumerator


parseUrl' :: String -> Request m
parseUrl' = fromJust . parseUrl


liftIteratee :: (Monad m, Monad n) => (m (Step a m b) -> n (Step a m b)) -> Iteratee a m b -> Iteratee a n b
liftIteratee f iter = Iteratee $ f (runIteratee iter) >>= check
  where
    check (Continue fi) = return $ Continue (liftIteratee f . fi)
    check (Yield r s)   = return $ Yield r s
    check (Error ex)    = return $ Error ex

liftIterateeIO :: (MonadIO m) => Iteratee a IO b -> Iteratee a m b
liftIterateeIO = liftIteratee liftIO


addHeaders :: [(Ascii, Ascii)] -> Request m -> Request m
addHeaders hs r = r { requestHeaders = map (first CI.mk) hs ++ requestHeaders r }

addQuery :: Query -> Request m -> Request m
addQuery qs r = r { queryString = qs ++ queryString r }

