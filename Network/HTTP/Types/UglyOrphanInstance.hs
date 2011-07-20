{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | This module exists just to expose an orphan instance of Show for Requests.
-- It would obviously fit better in the module the type is defined in, but
-- pending future code motion, meh. ...
-- 
module Network.HTTP.Types.UglyOrphanInstance () where

import Data.ByteString.Char8   (ByteString, unpack)
import Data.CaseInsensitive    (CI (..))
import Network.HTTP.Enumerator (Request (..))
import Network.HTTP.Types      (Ascii)

instance Show (Request m) where

    showsPrec _ r = unlines' $

                    [ ss "<"
                    . ss "http". (if secure r then ss "s" else id). ss "://"
                    . unpacks (host r) . ss ":" . shows (port r) . unpacks (path r)
                    . ss ">" ]

                    ++ nl ( showsDivider "query"   (map (push . qkv) (queryString r))
                         ++ showsDivider "headers" (map (push . hkv) (requestHeaders r)) )

ss :: String -> ShowS
ss = showString

nl :: [ShowS] -> [ShowS]
nl [] = []
nl xs = xs ++ [ss ""]


showsDivider :: String -> [ShowS] -> [ShowS]
showsDivider _   [] = []
showsDivider tag xs = (ss " -- " . ss tag . ss " -- ") : xs

qkv :: (ByteString, Maybe ByteString) -> ShowS
qkv (unpacks -> k, Just (unpacks -> v)) = k . ss " = " . v
qkv (unpacks -> k, Nothing            ) = k

hkv :: (CI Ascii, Ascii) -> ShowS
hkv (unpacks . original -> k, unpacks -> v) = k . ss " = " . v

push :: ShowS -> ShowS
push = (ss "  | " .)

unlines' :: [ShowS] -> ShowS
unlines' [] = id
unlines' xs = foldr1 (\l1 l2 -> l1 . showString "\n" . l2) xs

unpacks :: ByteString -> ShowS
unpacks = showString . unpack


