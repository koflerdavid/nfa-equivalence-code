module Language.Automata.HkntParser.Class where


import Data.List (intersperse)


type Transition = (String, Char, String)

data Operation = Inclusion | Equivalence
    deriving (Enum, Eq, Show)

type Check = ([String], Operation, [String])

data Result =
    Result { resTransitions :: [Transition]
           , resAcceptingStates :: [String]
           , resChecks :: [Check]
    }
    deriving (Eq, Show)

data Token = Identifier { tkIdentifierName :: String }
           | Accept
           | Check
           | Colon
           | Equals
           | GreaterEquals
           | Arrow { tkLabels :: [Char] }
           | Newline
           deriving (Eq)


instance (Show Token) where
    show (Identifier name) = name
    show Accept = "accept"
    show Check = "check"
    show Colon = ":"
    show Equals = "="
    show GreaterEquals = "=>"
    show (Arrow labels) = concat ["-", intersperse '+' labels, "->"]
    show Newline = "<newline>"
