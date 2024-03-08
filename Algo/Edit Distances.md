# Levenshtein distance

Informally, the Levenshtein distance between two words is the minimum number of single-character edits (insertions, deletions or substitutions) required to change one word into the other.

```haskell
lev :: String -> String -> Int
lev [] ys = length ys
lev xs [] = length xs
lev (x:xs) (y:ys) 
 | x == y    = lev xs ys
 | otherwise = 1 + min (lev xs (y:ys)) (lev (x:xs) ys)  

normalisedLev xs ys = (realToFrac $ lev xs ys) / (lengthOf xs ys)
```

# Damerau–Levenshtein distance

 Informally, the Damerau–Levenshtein distance between two words is the minimum number of operations (consisting of insertions, deletions or substitutions of a single character, or transposition of two adjacent characters) required to change one word into the other.

# Hamming Distance

The Hamming distance between two strings or vectors of equal length is the number of positions at which the corresponding symbols are different. In other words, it measures the minimum number of substitutions required to change one string into the other, or equivalently, the minimum number of errors that could have transformed one string into the other.

# Jaro–Winkler distance
#statistics 

The Jaro–Winkler distance uses a _prefix scale_ $p$ which gives more favourable ratings to strings that match from the beginning for a set prefix length $\ell$.

```haskell
jaro :: String -> String -> Double
jaro s1 s2 
 | matchingCharCount == 0 = 0
 | otherwise = sum 
     [ matchingCharCount / s1Length 
     , matchingCharCount / s2Length     
     , (matchingCharCount - transCount) / matchingCharCount 
     ] / 3
  _
 where
  -- num of chars matches within matchingDistanceThreshold
  matchingCharCount = ... 
  matchingDistanceThreshold :: Int
  matchingDistanceThreshold = max s1Length s2Length / 2 - 1
  s1Length = genericLength s1
  s2Length = genericLength s2
  -- transCount is the number of 
  -- matching characters that are not in 
  -- the right order divided by two.
  transCount = ...

jaroWinkle s1 s2 = sim + l * p * (1- sim)
 where 
  sim = jaro s1 s2
  l = -- length of common prefix at the start of the string up to a maximum of 4 characters
  p = 0.1
```
