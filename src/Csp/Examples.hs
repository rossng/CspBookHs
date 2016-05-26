module Csp.Examples where

import Csp.Csp as Csp

stop :: Process
stop = Process (const Nothing)

coin :: Event
coin = Ev "coin"

tick :: Event
tick = Ev "tick"

coinAccepter :: Process
coinAccepter = Process (\x -> case x of
                                (Ev "coin") -> Just stop
                                _           -> Nothing)

clock :: Process
clock = Process (\x -> case x of
                          (Ev "tick") -> Just clock
                          _           -> Nothing)
