{-# LANGUAGE RecordWildCards #-}

module Calculator.Time
    ( Time (..)
    , toSeconds
    , fromSeconds
    , multiply
    , divide
    ) where

import Text.Printf (printf)

data Time = Time
    { hours :: Int
    , minutes :: Int
    , seconds :: Int
    } deriving (Eq)
    

instance Show Time where
    show Time {..} = show hours <> ":" <> printf "%02d" minutes <> ":" <> printf "%02d" seconds


toSeconds :: Time -> Int
toSeconds Time {..} = seconds + minutes * 60 + hours * 60 * 60

fromSeconds :: Int -> Time
fromSeconds secs_ = Time { hours = hours_, minutes = minutes_, seconds = seconds_ }
    where
        (hours_, minutes__) = secs_ `divMod` (60 * 60)
        (minutes_, seconds_) = minutes__ `divMod` 60

multiply :: Double -> Time -> Time
multiply m t = fromSeconds $ round (m * fromIntegral (toSeconds t))

divide :: Double -> Time -> Time
divide m t = fromSeconds $ round (fromIntegral (toSeconds t) / m)