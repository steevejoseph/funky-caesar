import Data.Char
import Data.List
import Data.Maybe

full_let2nat phrase = map(\x -> ord x - ord 'a') phrase

let2nat charles = ord charles - ord 'a'

-- I believe chr turns an ordinal to its corresponding char val.
nat2let num =  chr (num + ord 'a')

shift num letter
    | isLower letter = nat2let (((let2nat letter) + num) `mod` 26)
    | otherwise = letter

encode rot plaintext = map(\x -> shift rot x) plaintext

decode rot ciphertext = map(\x -> shift (-1 * rot) x) ciphertext

lowers phrase = length (filter (isLower) phrase)

count charles phrase = length $ filter (== charles) phrase

percent numerator denominator = (fromIntegral numerator) /  (fromIntegral denominator ) * 100

table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

freqs phrase =  [percent (count c phrase) n | c <- ['a'..'z'] ] where n = lowers phrase

rotate n phrase = zipWith const (drop n (cycle phrase)) phrase

chisqr observed expected = sum [((observation - expectation )^2)/ expectation | (observation,expectation) <- zip observed expected]

-- rip, doesn't work.
-- position needle haystack = fromJust $ elemIndex needle haystack


position needle haystack = [ x | (needle', x) <- zip haystack [0..n], needle==needle' ]
                 where n = length haystack - 1

crack cs = encode (-rot) cs
           where rot = head (position (minimum chisqrtable) chisqrtable)
                 chisqrtable = [ chisqr (rotate n table') table | n <- [0..25] ]
                 table' = freqs cs
