module Sound.Tidal.Note where

import Sound.Tidal.Pattern
import Data.Char

baseNote 'A' = 81 -- A6
baseNote 'B' = 83 -- B6
baseNote 'C' = 84 -- C7
baseNote 'D' = 86
baseNote 'E' = 88
baseNote 'F' = 89
baseNote 'G' = 91 -- G7

-- Modifies a base note, given a string suffix which sharpens, flattens and/or changes its octave
noteSuffix :: Int -> String -> Int
noteSuffix base ([]) = base
noteSuffix base ('s':t) = noteSuffix (base + 1) t -- sharp
noteSuffix base ('b':t) = noteSuffix (base - 1) t -- flat
noteSuffix base oct | base >= baseNote 'C' = base + 12 * (octave - 7) -- octave change
                    | otherwise = base + 12 * (octave - 6)
                    where octave = read oct

-- Parse a full note string to a midi note. E.g., "As9" (A sharp in the 9th octave) = 118
fromName :: String -> Int
fromName s@(h:t) | n < 0 || n > 127 = error $ "note out of midi range: " ++ s
                 | otherwise = n
                 where n = noteSuffix (baseNote h) t

-- Convert a pattern of note names into a pattern of midi note numbers
fromNames :: Pattern String -> Pattern Int
fromNames p = fmap fromName p
