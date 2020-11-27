{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
-- | 

module Beelzebub.Imp.ExitCode
  (ExitCode, fromSystemType)
where

import qualified System.Exit as SE
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype ExitCode = ExitCode Int
  deriving (Eq, Generic)

fromSystemType :: SE.ExitCode -> ExitCode
fromSystemType = \case
  SE.ExitSuccess -> ExitCode 0
  SE.ExitFailure code -> ExitCode code

instance Show ExitCode where
  show (ExitCode 0) = "succeeded (exit code: 0)"
  show (ExitCode code) = "failed (exit code: " <> show code <> ")"

instance FromJSON ExitCode
instance ToJSON ExitCode
