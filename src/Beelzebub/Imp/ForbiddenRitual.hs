-- |
{-# LANGUAGE ForeignFunctionInterface #-}

module Beelzebub.Imp.ForbiddenRitual where

foreign import ccall "plus_ten" plusTen :: Int -> IO Int

data SummoningOptions = SummoningOptions
  { pwd :: ()
  , env :: ()
  -- TODO
  }

{-

-}
summoningRitual :: SummoningOptions -> IO ()
summoningRitual = undefined
