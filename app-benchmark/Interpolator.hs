module Interpolator
    ( Template
    , compile
    , unsafeInterpolate
    ) where

data Template a = Template Int [[a]]
    deriving (Show)

compile :: String -> Template Char
compile string = let parts = divideIntoParts string [] []
                 in
                     Template (length parts) parts
  where
    divideIntoParts :: String -> String -> [String] -> [String]
    divideIntoParts ('%' : '%' : xs) part acc =
        divideIntoParts xs [] (reverse part : acc)
    divideIntoParts ('%' : xs) part acc =
        divideIntoParts xs ('%' : part) acc
    divideIntoParts (x : xs) part acc =
        divideIntoParts xs (x : part) acc
    divideIntoParts [] part acc =
        reverse $ reverse part : acc

unsafeInterpolate :: Show a => Template a -> [[a]] -> [a]
unsafeInterpolate (Template n parts) args =
    if n /= length args + 1
    then error $ "Not enough arguments to insert"
    else
    -- The idea is to put an argument in front of every part.
    -- Since there is no argument in front of the first part, an empty string is inserted.
    concat $
        concat $
            zipWith (\x y -> [ x, y ]) ([] : args) parts
