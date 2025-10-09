{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Transmission.RPC.Enum
  (
    EncryptionMode
  , IdleMode
  , RatioLimitMode
  , Mode(..)
  )

where
import           Data.Aeson       (FromJSON, ToJSON (toJSON), Value (Number),
                                   withScientific, withText)
import           Data.Aeson.Types (FromJSON (parseJSON), prependFailure)

data Mode = Global | Single | Unlimited deriving Show

data EncryptionMode = Required | Preferred | Tolerated deriving Show

type IdleMode = Mode
type RatioLimitMode = Mode

instance ToJSON Mode where
  toJSON Global    = Number 0
  toJSON Single    = Number 1
  toJSON Unlimited = Number 2

instance FromJSON Mode where
  parseJSON = withScientific "Mode" $ \case
    0 -> pure Global
    1 -> pure Single
    2 -> pure Unlimited
    v -> prependFailure "parsing Mode failed: " (fail (show v ++ " is not a valid mode"))

instance ToJSON EncryptionMode where
  toJSON Required  = "required"
  toJSON Preferred = "preferred"
  toJSON Tolerated = "tolerated"

instance FromJSON EncryptionMode where
  parseJSON = withText "EncryptionMode" $ \case
    "required" -> pure Required
    "preferred" -> pure Preferred
    "tolerated" -> pure Tolerated
    v -> prependFailure "parsing EncryptionMode failed: " (fail (show v ++ " is not a valid encryption mode"))
