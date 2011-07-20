{-# LANGUAGE BangPatterns  #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.RFC3339 (
    parseRFC3339, readRFC3339, parseISODate, readISODate, showRFC3339
) where

import           Control.Applicative
import           Data.ByteString (ByteString)

import           Data.Char (toUpper, toLower)
import           Data.Attoparsec.Char8

import           Data.Time


-- parsers

readRFC3339 :: ByteString -> Maybe ZonedTime
readRFC3339 = qParse parseRFC3339

readISODate :: ByteString -> Maybe Day
readISODate = qParse parseISODate

qParse :: Parser a -> ByteString -> Maybe a
qParse p = either (const Nothing) Just . parseOnly p


parseRFC3339 :: Parser ZonedTime
parseRFC3339 = do
    (!day) <- parseISODate
    charCI 'T'
    (!tod) <- parseTODFloating
    (!tz ) <- zone
    return $! ZonedTime (LocalTime day tod) tz

parseISODate :: Parser Day
parseISODate = liftMaybe =<<
    fromGregorianValid <$> (decimal <* dash)
                       <*> (decimal <* dash)
                       <*> decimal

parseTODFloating :: Parser TimeOfDay
parseTODFloating = liftMaybe =<<
    makeTimeOfDayValid <$> (decimal <* semi)
                       <*> (decimal <* semi)
                       <*> (realToFrac <$> double)

zone :: Parser TimeZone
zone = (utc <$ charCI 'z') <|> (decodeTZ <$> signed decimal <* semi <*> decimal)
  where
    decodeTZ h m = minutesToTimeZone $ (h * 60) `op` m
        where op = if h >= 0 then (+) else (-)

semi, dash :: Parser ()
semi = () <$ char ':'
dash = () <$ char '-'

charCI :: Char -> Parser ()
charCI c = () <$ (char (toLower c) <|> char (toUpper c))

liftMaybe :: (Alternative f) => Maybe a -> f a
liftMaybe = maybe empty pure


-- serializers


-- XXX use blaze-builder?

showRFC3339 :: ZonedTime -> String
showRFC3339 (ZonedTime t tz) = formatTime undefined "%FT%T%Q" t ++ showTZ tz

showTZ :: TimeZone -> String
showTZ (TimeZone 0   _ _) = "Z"
showTZ (TimeZone off _ _) = prefix : (sh $ abs off `div` 60) ++ ':' : (sh $ abs off `mod` 60)
  where
    prefix | off < 0   = '-'
           | otherwise = '+'

    sh x = case show x of
                x'@[_] -> '0' : x'
                x'     ->       x'

