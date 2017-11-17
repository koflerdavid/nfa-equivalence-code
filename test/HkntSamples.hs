module HkntSamples where

import Data.Nfa

introductionInHkntFormat :: String
introductionInHkntFormat =
    "x -a-> y\n\
    \y -a-> z\n\
    \z -a-> x y\n\
    \u -a-> w v\n\
    \v -a-> w\n\
    \w -a-> u\n\
    \accept: y v\n\
    \check: x = u\n"

introductionNfa1 :: Nfa Char
introductionNfa1 =
    let (x, y, z) = (0, 1, 2) :: (Int, Int, Int)
    in buildNfa
           [y]
           [((x, 'a'), [y]), ((y, 'a'), [z]), ((z, 'a'), [x]), ((z, 'a'), [y])]

introductionNfa2 :: Nfa Char
introductionNfa2 =
    let (u, v, w) = (0, 1, 2) :: (Int, Int, Int)
    in buildNfa
           [v]
           [((u, 'a'), [v]), ((u, 'a'), [w]), ((v, 'a'), [w]), ((w, 'a'), [u])]

type Int6 = (Int, Int, Int, Int, Int, Int)

introductionNfaMerged =
    let (u, v, w, x, y, z) = (0, 1, 2, 3, 4, 5) :: Int6
    in buildNfa
           [v, y]
           [ ((u, 'a'), [v])
           , ((u, 'a'), [w])
           , ((v, 'a'), [w])
           , ((w, 'a'), [u])
           , ((x, 'a'), [y])
           , ((y, 'a'), [z])
           , ((z, 'a'), [x])
           , ((z, 'a'), [y])
           ]

figure3NfaMerged =
    let (u, x, y, z) = (0, 1, 2, 3) :: (Int, Int, Int, Int)
    in buildNfa
           [u, x, y]
           [ ((u, 'a'), [u])
           , ((x, 'a'), [y, z])
           , ((y, 'a'), [x])
           , ((z, 'a'), [y])
           ]
