{-# LANGUAGE OverloadedStrings, GADTs, ExistentialQuantification, DeriveDataTypeable  #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PackageImports  #-}

{- |
Module      :  Web.Google.HTTP
Copyright   :  © David Kaloper, 2011
License     :  BSD-style

Maintainer  :  David Kaloper <dkaloper@mjesec.ffzg.hr>
Stability   :  experimental
Portability :  Requires several GHC-only extensions.

This module exports the network operations of the package.
-}

module Web.Google.HTTP (
      googleOAuth, GAPICred (..), GAPIError (..)
    , jsonIter, parsingIter, simpleIter, oauthIter
    , catchGapi
    , addHeaders, addQuery
) where


import           Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI (mk)
import           Data.Typeable (Typeable (..))

import           Control.Monad

import           Control.Monad.IO.Class (MonadIO (..))
import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as CIO
import           Control.Exception

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8 (unpack)

import           Data.Enumerator hiding (map)

import           Web.Authenticate.OAuth

import           Network.HTTP.Enumerator
import           Network.HTTP.Types
import           Network.HTTP.Types.UglyOrphanInstance ()

import           Data.Aeson.Parser (json)
import           Data.Aeson.Types (Value (..), Parser, parseEither)
import           Data.Attoparsec.Enumerator (iterParser, ParseError)

import           Web.Google.Utilities

-- | Construct a Google-specific OAuth consumer.
googleOAuth :: String                         -- ^ 'oauthServerName'
            -> SignMethod                     -- ^ Google doesn't support 'PLAINTEXT'.
            -> Maybe (ByteString, ByteString) -- ^ Consumer key and consumer secret. Both default to "anonymous".
            -> Maybe ByteString               -- ^ The callback.
            -> [ByteString]                   -- ^ List of endpoints to authorize for. Google's "scope" parameter.
            -> OAuth
googleOAuth serverName signatureMethod consumerCreds callback scope =
    OAuth { oauthServerName      = serverName
          , oauthSignatureMethod = signatureMethod
          , oauthConsumerKey     = consumerKey
          , oauthConsumerSecret  = consumerSecret
          , oauthCallback        = callback
          , oauthScope           = scope
          , oauthRequestUri      = "https://www.google.com/accounts/OAuthGetRequestToken"
          , oauthAuthorizeUri    = "https://www.google.com/accounts/OAuthAuthorizeToken"
          , oauthAccessTokenUri  = "https://www.google.com/accounts/OAuthGetAccessToken"
          }
  where
    (consumerKey, consumerSecret) = fromMaybe ("anonymous", "anonymous") consumerCreds


-- | Data type for Google API OAuth authenticated consumer (per-app 'OAuth' plus per-user 'Credential')
data GAPICred = GAPICred OAuth Credential

-- | The type of GAPI exceptions
data GAPIError where
    JSONParseException     :: ParseError -> GAPIError
    JSONFormatException    :: String -> Value -> GAPIError
    RedirDepthException    :: forall m. Request m -> ResponseHeaders -> GAPIError
    RedirLocationException :: forall m. Request m -> Status -> ResponseHeaders -> GAPIError
    ResponseError          :: forall m. Request m -> Response -> GAPIError
    InvalidCredentials     :: Ascii -> GAPIError
    deriving (Typeable)

instance Show GAPIError where
    show (JSONParseException     e    ) = "JSONParseException " ++ show e
    show (JSONFormatException    s v  ) = "JSONFormatException \"" ++ s ++ "\" " ++ show v
    show (RedirDepthException    r h  ) = "RedirDepthException " ++ show r ++ " " ++ show h
    show (RedirLocationException r s h) = "RedirLocationException "  ++ show r ++ " " ++ show s ++ " " ++ show h
    show (ResponseError          r rs ) = "ResponseError " ++ show r ++ " " ++ show rs
    show (InvalidCredentials     e    ) = "InvalidCredentials \"" ++ show e ++ "\""

instance Exception GAPIError


-- resp. code. predicates
httpCodeOK, httpCodeRedir :: Int -> Bool
httpCodeOK    code = code >= 200 && code < 300
httpCodeRedir code = code >= 300 && code < 400

-- Drain input and throw is as an error.
consumeInputOnError :: Monad m => Request n -> Status -> ResponseHeaders -> Iteratee ByteString m a
consumeInputOnError req status headers = lbsIter status headers >>= throwError . ResponseError req

-- Iteratee consuming and parsing JSON input, with parse errors propagates as a
-- GAPIError.
jsonIter :: Monad m => Iteratee ByteString m Value
jsonIter = iterParser json `catchError` (throwError . wrapJSONExcepton)

wrapJSONExcepton :: SomeException -> SomeException
wrapJSONExcepton e = maybe e (toException . JSONParseException) $ fromException e

-- | This is really just Aeson `parse' lifted into iteratee. It is NOT fed Value
-- to parse using iteratee mechanics, because aeson does not incrementally parse
-- anyway. Instead, it embeds errors into GAPIError and propagates them through
-- the iteratee.
parsingIter :: (MonadIO m) => (Value -> Parser a) -> Value -> Iteratee t m a
parsingIter parser value = either (throwError . (`JSONFormatException` value)) return
                                  (parser `parseEither` value)

-- | Just make a single HTTP fetch and read that as JSON.
simpleIter :: MonadIO m => Request IO -> Manager -> Iteratee ByteString m Value
simpleIter req manager = liftIterateeIO $ flip (http req) manager $ \status headers ->
    case status of
         Status code _ | httpCodeOK code -> jsonIter
         _                               -> consumeInputOnError req status headers

-- | Iteratee using OAuth and following a single redirect. Reads output as JSON.
oauthIter :: (MonadIO m) => GAPICred -> Request IO -> Query -> Manager -> Iteratee ByteString m Value
oauthIter (GAPICred oa cred) req0 q manager = liftIterateeIO $ comm (1 :: Int) req1
  where
    req1 = q `addQuery` req0

    comm redir req = do
        req' <- tryIO $ signOAuth oa cred req
        flip (http req') manager $ \status headers ->

            -- XXX FIXME This would be the place to detect various proto-level
            -- error conditions, such as expired creds et al.
            case (status, nextUrl headers) of
                 (Status 401 err, _)           -> throwError $ InvalidCredentials err
                 (Status code _ , _)
                    | httpCodeOK code          -> jsonIter
                 (Status code _ , _)
                    | not (httpCodeRedir code) -> consumeInputOnError req' status headers
                 (_, Just nextReq)
                    | redir > 0                -> comm (redir - 1) nextReq
                    | otherwise                -> throwError $ RedirDepthException req' headers
                 (_, Nothing)                  -> throwError $ RedirLocationException req' status headers

    nextUrl = parseUrl . BS8.unpack <=< lookup (CI.mk "location")

-- | A bracket for gapi calls, catching exactly the exceptions gapis can raise.
catchGapi :: (CIO.MonadCatchIO m) => (GAPIError -> m a) -> (IOException -> m a) -> m a -> m a
catchGapi gapihandler iohandler gapicall = gapicall `CIO.catch` gapihandler `CIO.catch` iohandler


