{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Google.GCal (

      EventAccess, eaPublic, eaPrivate

    , EventProjection, epFull, epFullNoAttendees, epComposite, epAttendeesOnly, epFreeBusy, epBasic

    , qCTZ, qFields 
    , qMaxattendees, qMaxresults 
    , qFutureevents, qSingleevents, qShowdeleted, qShowhidden, qSortAscending
    , qOrderByLastmod, qOrderByStart 
    , qReccurenceStart, qReccurenceStop, qStartMin, qStartMax, qUpdatedMin 

    , eventsURL, allCalendarsURL
    , gcalIter, gcal, gcalWith

) where

import           Data.List (intercalate)

import           Data.Time
import           Data.Time.RFC3339

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)

import           Data.Enumerator (Iteratee, run_)

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson.Types (Value, Parser)

import           Network.HTTP.Enumerator (Request, Manager)
import           Network.HTTP.Types

import           Web.Google.HTTP
import           Web.Google.Utilities


data EventAccess = EA { ea :: String } deriving (Show, Eq)

eaPublic, eaPrivate :: EventAccess
eaPublic  = EA "public"
eaPrivate = EA "private"

data EventProjection = EP { ep :: String } deriving (Show, Eq)

epFull, epFullNoAttendees, epComposite, epAttendeesOnly, epFreeBusy, epBasic :: EventProjection
epFull            = EP "full"
epFullNoAttendees = EP "full-noattendees"
epComposite       = EP "composite"
epAttendeesOnly   = EP "attendees-only"
epFreeBusy        = EP "free-busy"
epBasic           = EP "basic"

boolQuery :: ByteString -> Bool -> Query
boolQuery name val = [(name, Just $ if val then "true" else "false")]

altQuery :: ByteString -> (ByteString, ByteString) -> Bool -> Query
altQuery name (a1, a2) val = [(name, Just $ if val then a1 else a2)]

timeQuery :: ByteString -> ZonedTime -> Query
timeQuery name time = [(name, Just . BS8.pack $ showRFC3339 time)]

integralQuery :: (Integral a) => ByteString -> a -> Query
integralQuery name n = [(name, Just . BS8.pack . show $ n)]

qCTZ, qFields :: ByteString -> Query
qCTZ zone    = [("ctz"   , Just zone)]
qFields flds = [("fields", Just flds)]

qMaxattendees, qMaxresults :: (Integral a) => a -> Query
qMaxattendees = integralQuery "max-attendees"
qMaxresults   = integralQuery "max-results"

qFutureevents, qSingleevents, qShowdeleted, qShowhidden, qSortAscending :: Bool -> Query
qFutureevents  = boolQuery "futureevents"
qSingleevents  = boolQuery "singleevents"
qShowdeleted   = boolQuery "showdeleted"
qShowhidden    = boolQuery "showhidden"
qSortAscending = altQuery  "sortorder" ("ascending", "descending")

qOrderByLastmod, qOrderByStart :: Query
qOrderByLastmod = [("orderby", Just "lastmodified")]
qOrderByStart   = [("orderby", Just "starttime"   )]

qReccurenceStart, qReccurenceStop, qStartMin, qStartMax, qUpdatedMin :: ZonedTime -> Query
qReccurenceStart = timeQuery "recurrence-expansion-start"
qReccurenceStop  = timeQuery "recurrence-expansion-end"
qStartMin        = timeQuery "start-min"
qStartMax        = timeQuery "start-max"
qUpdatedMin      = timeQuery "updated-min"

eventsURL :: EventAccess -> EventProjection -> Request m
eventsURL acc proj = url [ "https://www.google.com/calendar/feeds/default", ea acc, ep proj ]
  where
    url = parseUrl' . intercalate "/"

allCalendarsURL :: Request m
allCalendarsURL = parseUrl' "https://www.google.com/calendar/feeds/default/allcalendars/full"


gcalIter :: (MonadIO m) => GAPICred -> Request IO -> Query -> Manager -> Iteratee ByteString m Value
gcalIter gapicred req q manager = oauthIter gapicred (annoint req) q manager
  where
    annoint = addQuery [("alt", Just "jsonc")] . addHeaders [("GData-Version", "2")]

gcal :: (MonadIO m) => GAPICred -> Request IO -> Query -> Manager -> m Value
gcal gapicred req q manager = run_ $ gcalIter gapicred req q manager

gcalWith :: (MonadIO m) => GAPICred -> Request IO -> Query -> Manager -> (Value -> Parser a) -> m a
gcalWith gapicred req qs manager parser = run_ $ gcalIter gapicred req qs manager >>= parsingIter parser

