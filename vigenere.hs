-- Vigenère Chiffre encoder and decoder

import Data.Char

encode :: String -> String -> String
encode text key = zipWith rot text (cycle key)
  where
    rot :: Char -> Char -> Char
    rot t k = chr $ let x = ord t + (ord k - ord 'A') in if x > ord 'Z' then x - 26 else x

decode :: String -> String -> String
decode text key = zipWith rot text (cycle key)
  where
    rot :: Char -> Char -> Char
    rot t k = chr $ let x = ord t - (ord k - ord 'A') in if x < ord 'A' then x + 26 else x

{-
All strings must be regex [A-Z]

encode "DASISTEINLANGERBEISPIELTEXT" "CTMAGAZIN"
"FTEIYTDQANTZGKRAMVUIUERTDFG"

decode "FTEIYTDQANTZGKRAMVUIUERTDFG" "CTMAGAZIN"
"DASISTEINLANGERBEISPIELTEXT"
-}
