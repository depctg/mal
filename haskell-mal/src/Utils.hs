module Utils where

maybeToEither = flip maybe Right . Left

evens (x:xs) = x:odds xs
evens _ = []

odds (_:xs) = evens xs
odds _ = []

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing = id
orElse a = const a