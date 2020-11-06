-- | 
{-# LANGUAGE OverloadedStrings #-}

module Beelzebub.Wireable where

import Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import Data.String (IsString)

keyKey :: IsString a => a
keyKey = "key"

valueKey :: IsString a => a
valueKey = "value"

data (ToJSON a, FromJSON a) => Wireable a =
  { name :: Text , payload :: a }

instance ToJSON Wireable where
  toJSON wireable = Aeson.object
    [ keyKey .= (name wireable)
    , valueKey .= (Aeson.toJSON $ payload wireable)
    ]

  toEncoding wireable = Aeson.pairs
    (keyKey .= (name wireable) <>
     valueKey .= (Aeson.toEncoding $ payload wireable)
    )

instance FromJSON Wireable where
  parseJSON (AesonTypes.Object v) = decode <$> v .: keyKey <*> v .: valueKey
  parseJSON invalid =
    AesonTypes.prependFailure "parsing wireable failed, "
      (AesonTypes.typeMismatch "Object" invalid)
  

-- Data.Aeson.fromJSON :: FromJSON a => Value -> Result a
-- Data.Aeson.Parser.json :: Parser ByteString Value
-- Data.Conduit.AttoParsec.conduitParserEither :: (MonadThrow m) =>
--    Parser ByteString b -> ConduitT ByteString (Either ParseError (PositionRange, b)) m ()
