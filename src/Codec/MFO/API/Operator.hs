{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}
module Codec.MFO.API.Operator
    ( OReqAuth (..)
    , OAnswerAuth (..)
    , OReqNews(..)
    , OAnswerNews(..)
    , NewsShort(..)
    , NewsFull(..)
    )
    where

import Data.Aeson
import Data.Aeson.Types as AT
import Data.Text
import Control.Applicative
import Control.Monad
import Data.Time


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
    parseJSON parent@(Object v) = getType v parseOReqAuth
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
    parseJSON parent@(Object v) = getType v parseOAnswerAuth
    parseJSON _ = mzero

parseOAnswerAuth "OAANeedPin" v = pure OAANeedPin
parseOAnswerAuth "OAAError" v = OAAError <$> v .: "reason"
parseOAnswerAuth "OAALoggedIn" v = OAALoggedIn <$> v .: "fullname"
parseOAnswerAuth _ _ = mzero

getType:: (FromJSON a) => Object-> (Text-> Object-> AT.Parser a)-> AT.Parser a
getType v f = do
    v .: "type" >>= \(String dtype) -> f dtype v


-- instance ToJSON LocalTime where
--     toJSON lt
{-| represents short version of news
 -
 -}
data NewsShort = NewsShort
    { nsId:: Text
    , nsPostDate:: UTCTime
    , nsTopic:: Text
    , nsTags:: Text
    , nsShortContent:: Text
    }
    deriving Show
instance ToJSON NewsShort where
    toJSON NewsShort {..} = object
        [ "type" .= ("NewsShort":: Text)
        , "id" .= nsId
        , "postdate" .= nsPostDate
        , "topic" .= nsTopic
        , "tags" .= nsTags
        , "short" .= nsShortContent
        ]
instance FromJSON NewsShort where
    parseJSON (Object v) = getType v parseNewsShort
    parseJSON _ = mzero

parseNewsShort "NewsShort" v = NewsShort
    <$> v .: "id"
    <*> v .: "postdate"
    <*> v .: "topic"
    <*> v .: "tags"
    <*> v .: "short"
parseNewsShort _ _ = mzero

data NewsFull = NewsFull
    { nfId :: Text
    , nfPostDate:: UTCTime
    , nfTopic:: Text
    , nfTags:: Text
    , nfShortContent:: Text
    , nfContent:: Text
    }
    deriving Show
instance ToJSON NewsFull where
    toJSON NewsFull {..} = object
        [ "type" .= ("NewsFull":: Text)
        , "id" .= nfId
        , "postdate" .= nfPostDate
        , "topic" .= nfTopic
        , "tags" .= nfTags
        , "short" .= nfShortContent
        , "content" .= nfContent
        ]
instance FromJSON NewsFull where
    parseJSON (Object v) = getType v parseNewsFull
    parseJSON _ = mzero

parseNewsFull "NewsFull" v = NewsFull
    <$> v .: "id"
    <*> v .: "postdate"
    <*> v .: "topic"
    <*> v .: "tags"
    <*> v .: "short"
    <*> v .: "content"


data OReqNews
    = ORNGetList
        { ornStartFrom:: Int
        , ornCount:: Int
        }
    | ORNAddNews
        { ornTitle:: Text
        , ornShortContent:: Text
        , ornContent:: Text
        }
   deriving Show
instance ToJSON OReqNews where
    toJSON ORNGetList {..} = object
        [ "type" .= ("ORNGetList" :: Text)
        , "startfrom" .= ornStartFrom
        , "count" .= ornCount
        ]
    toJSON ORNAddNews {..} = object
        [ "type" .= ("ORNAddNews":: Text)
        , "title" .= ornTitle
        , "shortcontent" .= ornShortContent
        , "content" .= ornContent
        ]
instance FromJSON OReqNews where
    parseJSON (Object v) = getType v parseOReqNews
    parseJSON _ = mzero
parseOReqNews "ORNGetList" v = ORNGetList
    <$> v .: "startfrom"
    <*> v .: "count"
parseOReqNews "ORNAddNews" v = ORNAddNews
    <$> v .: "title"
    <*> v .: "shortcontent"
    <*> v .: "content"
parseOReqNews _ _ = mzero


data OAnswerNews
    = OANList
        { oanListNews:: [NewsShort]
        }
    | OANError
        { oanErrorReason:: Text
        }
    | OANAdded
    | OANDeleted
   deriving Show

instance ToJSON OAnswerNews where
    toJSON OANList {..} = object
        [ "type" .= ("OANList":: Text)
        , "items" .= oanListNews
        ]
    toJSON OANError {..} = object
        [ "type" .= ( "OANError" :: Text)
        , "reason" .= oanErrorReason
        ]
    toJSON OANAdded = object
        [ "type" .= ("OANAdded":: Text)
        ]
    toJSON OANDeleted = object
        [ "type" .= ("OANDeleted" :: Text)
        ]
instance FromJSON OAnswerNews where
    parseJSON (Object v ) = getType v parseOAnswerNews
    parseJSON _ = mzero
parseOAnswerNews "OANList" v = OANList
    <$> v .: "items"
parseOAnswerNews "OANError" v = OANError
    <$> v .: "reason"
parseOAnswerNews "OANAdded" v = pure OANAdded
parseOAnswerNews "OANDeleted" v = pure OANDeleted
parseOAnswerNews _ _ = mzero

