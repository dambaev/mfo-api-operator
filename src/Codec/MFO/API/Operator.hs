{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}
module Codec.MFO.API.Operator
    ( OReqAuth (..)
    , OAnswerAuth (..)
    )
    where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad


{-| API Auth
 -
 -}
data OReqAuth
    = ORALogin
        { oraLogin:: Text
        , oraPassword:: Text
        }
    | ORAPin
        { oraPin:: Text
        }
    deriving (Eq,Show)

instance ToJSON OReqAuth where
    toJSON ORALogin {..} = object
        [ "type" .= ("ORALogin"::Text)
        , "login" .= oraLogin
        , "password" .= oraPassword
        ]
    toJSON ORAPin {..} = object
        [ "type" .= ("ORAPin"::Text)
        , "pin" .= oraPin
        ]

instance FromJSON OReqAuth where
    parseJSON parent@(Object v) = do
        v .: "type" >>= \(String dtype) -> parseOReqAuth dtype v
    parseJSON _ = mzero

parseOReqAuth "ORALogin" v = ORALogin
    <$> v .: "login"
    <*> v .: "password"
parseOReqAuth "ORAPin" v = ORAPin
    <$> v .: "pin"
parseOReqAuth _ _ = mzero



data OAnswerAuth
    = OAANeedPin
    | OAAError
        { oaaReason:: Text
        }
    | OAALoggedIn
        { oaaFullName:: Text
        }
    deriving (Show, Eq)

instance ToJSON OAnswerAuth where
    toJSON OAANeedPin = object [ "type" .= ("OAANeedPin":: Text) ]
    toJSON OAAError {..} = object
        [ "type" .= ("OAAError":: Text)
        , "reason" .= oaaReason
        ]
    toJSON OAALoggedIn {..} = object
        [ "type" .= ("OAALoggedIn" :: Text)
        , "fullname" .= oaaFullName
        ]
instance FromJSON OAnswerAuth where
    parseJSON parent@(Object v) = do
        v .: "type" >>= \(String dtype) -> parseOAnswerAuth dtype v
    parseJSON _ = mzero

parseOAnswerAuth "OAANeedPin" v = pure OAANeedPin
parseOAnswerAuth "OAAError" v = OAAError <$> v .: "reason"
parseOAnswerAuth "OAALoggedIn" v = OAALoggedIn <$> v .: "fullname"
parseOAnswerAuth _ _ = mzero

getType v f = do
    v .: "type" >>= \(String dtype) -> f dtype v

{-| represents short version of news
 -
 -}
data NewsShort = NewsShort
    { nsPostDate:: Date
    , nsTopic:: Text
    , nsTags:: Text
    }
instance ToJSON NewsShort where
    toJSON NewsShort {..} = object
        [ "type" .= ("NewsShort":: Text)
        , "postdate" .= nsPostDate
        , "topic" .= nsTopic
        , "tags" .= nsTags
        ]
instance FromJSON NewsShort instance
    parseJSON (Object v) = getType v parseNewsShort
    parseJSON _ = mzero

parseNewsShort "NewsShort" v = NewsShort
    <$> v .: "postdate"
    <*> v .: "topic"
    <*> v .: "tags"
parseNewsShort _ _ = mzero


data OReqNews
    = ORNGetList
        { ornStartFrom:: Int
        , ornCount:: Int
        }
instance ToJSON OReqNews where
    toJSON ORNGetList {..} = object
        [ "type" .= ("ORNGetList" :: Text)
        , "startfrom" .= ornStartFrom
        , "count" .= ornCount
        ]
instance FromJSON OReqNews where
    parseJSON (Object v) = getType v parseOReqNews
    parseJSON _ = mzero
parseOReqNews "ORNGetList" v = ORNGetList
    <$> v .: "startfrom"
    <*> v .: "count"
parseOReqNews _ _ = mzero


data OAnswerNews
    = OANList
        { oanListNews:: [NewsShort]
        }
    | OANError
        { oanErrorReason:: Text
        }

instance ToJSON OAnswerNews where
    toJSON OANList {..} = object
        [ "type" .= ("OANList":: Text)
        , "items" .= oanListNews
        ]
    toJSON OANError {..} = object
        [ "type" .= ( "OANError" :: Text)
        , "reason" .= oanErrorReason
        ]
    toJSON _ = mzero
instance FromJSON OAnswerNews where
    parseJSON (Object v ) = getType v parseOAnswerNews
    parseJSON _ = mzero
parseOAnswerNews "OANList" v = OANList
    <$> v .: "items"
parseOAnswerNews "OANError" v = OANError
    <$> v .: "reason"
parseOAnswerNews _ _ = mzero