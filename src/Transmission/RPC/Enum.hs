{-# LANGUAGE LambdaCase #-}
module Transmission.RPC.Enum 
  (
    IdleMode
  , RatioLimitMode
  , Mode(..)
  )

where 
import Data.Aeson (ToJSON (toJSON), Value (Number), FromJSON, withScientific)
import Data.Aeson.Types (FromJSON(parseJSON), prependFailure)

data Mode = Global | Single | Unlimited deriving Show

type IdleMode = Mode
type RatioLimitMode = Mode

instance ToJSON Mode where
  toJSON Global = Number 0
  toJSON Single = Number 1
  toJSON Unlimited = Number 2

instance FromJSON Mode where
  parseJSON = withScientific "Mode" $ \case
    0 -> pure Global
    1 -> pure Single
    2 -> pure Unlimited
    v -> prependFailure "parsing Mode failed: " (fail (show v ++ " is not a valid mode"))
