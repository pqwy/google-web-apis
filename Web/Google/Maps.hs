{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Maps (
    distanceIter, distance, distanceWith
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 (intercalate)

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Enumerator (Iteratee, run_)

import           Data.Aeson.Types (Value, Parser)

import           Network.HTTP.Enumerator (Request, Manager)
import           Network.HTTP.Types

import           Web.Google.HTTP
import           Web.Google.Utilities 


distanceUrl :: Request m
distanceUrl = parseUrl' "http://maps.googleapis.com/maps/api/distancematrix/json"

distanceQ :: [ByteString] -> [ByteString] -> Bool -> Query -> Request m
distanceQ origins destinations sensor qs = addQuery (qs' ++ qs) distanceUrl
  where
    qs' = [ ("origins"      , Just $ pack origins                      )
          , ("destinations" , Just $ pack destinations                 ) 
          , ("sensor"       , Just $ if sensor then "true" else "false") ]

    pack = BS8.intercalate "|"

--  -- | Distance matrix will limit a) URLs to 2048 chars (sans URL encoding), b)
--  -- queries to 100 elts each (origins * destinations), c) throttle queries to 100
--  -- elts per 10 seconds, and d) limit them to 2500 elts per 24 hrs.
--  -- This functions attempts to break down required sources and destinations not
--  -- to exceed per-query limit, and emits a series of queries, along with their
--  -- elements counts, that reconstruct the original one.
--  packDistances :: [ByteString]   -- ^ Sources
--                -> [ByteString]   -- ^ Destinations
--                -> [(Int, [ByteString], [ByteString])] -- ^ src+dest pairings
--  packDistances = 
--    where
--      overhead = 

distanceIter :: (MonadIO m) => [ByteString] -> [ByteString] -> Query -> Manager -> Iteratee ByteString m Value
distanceIter origins destinations qs = simpleIter (distanceQ origins destinations False qs)

distance :: (MonadIO m) => [ByteString] -> [ByteString] -> Query -> Manager -> m Value
distance origins destinations qs mgr = run_ $ distanceIter origins destinations qs mgr

distanceWith :: (MonadIO m) => [ByteString] -> [ByteString] -> Query -> Manager -> (Value -> Parser a) -> m a
distanceWith origins destinations qs mgr parser = run_ $ distance origins destinations qs mgr >>= parsingIter parser



