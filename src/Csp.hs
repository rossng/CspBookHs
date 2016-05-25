module Csp where

import Text.Show.Functions
import Data.Set as S

data Event = Ev String deriving (Eq, Ord, Show)
newtype Process = Process { run :: Event -> Maybe Process}
                  deriving (Show)
type Trace = [Event]

stop :: Process
stop = Process (const Nothing)

coin :: Event
coin = Ev "coin"

coinAccepter :: Process
coinAccepter = Process (\x -> case x of
                                (Ev "coin") -> Just stop
                                _           -> Nothing)

prefix :: Event -> Process -> Process
prefix c p = Process (\x -> if x == c then Just p else Nothing)

(⌒) :: Trace -> Trace -> Trace
(⌒) = (++)

catenate :: Trace -> Trace -> Trace
catenate = (⌒)

(↾) :: Trace -> S.Set Event -> Trace
(↾) t es = Prelude.filter (`S.member` es) t

restrict :: Trace -> S.Set Event -> Trace
restrict = (↾)

(↓) :: Trace -> Event -> Int
(↓) s x = length $ s ↾ S.singleton x
